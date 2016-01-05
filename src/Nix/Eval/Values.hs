{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Nix.Eval.Values where

import Nix.Common
import Nix.Eval.Expressions
import Nix.Eval.Constants
import qualified Data.HashMap.Strict as H
import qualified Data.Set as S
import qualified Data.Text as T

-------------------------------------------------------------------------------
--------------------------------- Values --------------------------------------
-------------------------------------------------------------------------------

-- | The type of runtime values.
data Value m
  = VConstant Constant
  -- ^ Constant values (isomorphic to constant expressions).
  | VAttrSet (AttrSet m)
  -- ^ Attribute set values.
  | VList (Seq (LazyValue m))
  -- ^ List values.
  | VFunction Text (Closure m)
  -- ^ Functions, with a parameter and a closure.
  | VNative (Native m)
  -- ^ Native values, which can be either values or functions.
  deriving (Generic)

instance Show (Value m) where
  show (VConstant c) = "VConstant (" <> show c <> ")"
  show (VAttrSet s) = show s
  show (VList vs) = show vs
  show (VFunction param closure) = concat [ unpack param, " => ("
                                          , show closure, ")"]
  show (VNative (NativeValue rv)) = show rv
  show (VNative _) = "(native function)"

instance Eq (Value m) where
  VConstant c == VConstant c' = c == c'
  VAttrSet as == VAttrSet as' = as == as'
  VList vs == VList vs' = vs == vs'
  VFunction p1 e1 == VFunction p2 e2 = p1 == p2 && e1 == e2
  VNative (NativeValue nv) == VNative (NativeValue nv') = nv == nv'
  _ == _ = False

instance IsString (Value m) where
  fromString = VConstant . fromString

instance FromConstant (Value m) where
  fromConstant = VConstant
  fromConstants = listV . map fromConstant
  fromConstantSet set = VAttrSet $ Environment $ map fromConstant set

-------------------------------------------------------------------------------
------------------------------ Lazy Values ------------------------------------
-------------------------------------------------------------------------------

-- | The result of evaluation: it might be an error. In Haskell, this
-- has the effect of creating lazy evaluation, as what is /inside/ the
-- result is not evaluated until inspected at some point.
newtype Result m a = Result (m (Either (EvalError m) a))
  deriving (Eq, Show, Functor, Applicative, Monad, Generic)

-- | In practice, this is pretty much the only result type that we'll
-- use, but we want to be able to use the Result as a monad, hence the
-- @newtype@ above.
type LazyValue m = Result m (Value m)

instance FromConstant (LazyValue m) where
  fromConstant = validR . fromConstant
  fromConstants = validR . fromConstants
  fromConstantSet = validR . fromConstantSet

-- | Tests if a lazy value is an error (forces WHNF evaluation)
isError :: LazyValue m -> Bool
isError (Result (Left _)) = True
isError _ = False

-- | Synonym for monadic bind, applying a function inside of a
-- 'Result', provided the 'Result' contains a 'Value'.
unwrapAndApply :: (Value m -> LazyValue m) -> LazyValue m -> LazyValue m
unwrapAndApply = (=<<)

-------------------------------------------------------------------------------
--------------------------- Deep Evaluation -----------------------------------
-------------------------------------------------------------------------------

-- | Most of the time we evaluate things lazily, but sometimes we want
-- to be able to evaluate strictly (esp. in the `deepSeq` function).
deeplyEval :: Value m -> LazyValue m
deeplyEval v = case v of
  VNative (NativeValue lval) -> deeplyEvalLazy lval
  VList lvals -> do
    -- Recur on items of the list, and check if any are errors.
    let (_, errs) = break (isError . deeplyEvalLazy) lvals
    case toList errs of
      [] -> validR v -- no errors, can just return the thing
      (err:_) -> err -- return the first error found
  VAttrSet (Environment lvals) -> do
    let (_, errs) = break (isError . deeplyEvalLazy) $ H.elems lvals
    case errs of
      [] -> validR v -- no errors, can just return the thing
      (err:_) -> err -- return the first error found
  -- Note that we don't recur into functions (or NativeFunctions), and
  -- since constants can't fail there's no need to handle them either.
  _ -> validR v

deeplyEvalLazy :: LazyValue m -> LazyValue m
deeplyEvalLazy err@(Result (Left _)) = err
deeplyEvalLazy (Result (Right v)) = deeplyEval v

-------------------------------------------------------------------------------
------------------------------- Native Values ---------------------------------
-------------------------------------------------------------------------------


-- | An embedding of raw values. Lets us write functions in Haskell
-- which operate on Nix values, and expose these in Nix code.
data Native m
  = NativeValue (LazyValue m)
  -- ^ A terminal value (or an error)
  | NativeFunction (LazyValue m -> Native m)
  -- ^ A function which lets us take the "next step" given a value.
  deriving (Generic)

-- | Types which can be turned into 'Native' values.
class Natify t where
  natify :: t -> Native m

instance Natify (Native m) where
  natify = id

-- | Of course, this applies to lazy values.
instance Natify (LazyValue m) where
  natify res = NativeValue res

-- | This is where things get interesting: a 'Natify' instance for
-- functions on lazy values lets us embed any function on 'LazyValue m's
-- as a 'Native' function. So for example, we can 'natify' a function
-- of type @'LazyValue m' -> -- 'LazyValue m'@, or @'LazyValue m' ->
-- 'LazyValue m' -> 'LazyValue m'@, etc.
instance Natify t => Natify (LazyValue m -> t) where
  natify function = NativeFunction $ \lval -> natify (function lval)

-- | We can 'natify' an arbitrary function on values (provided its
-- return type implements 'Natify'). However, it has the effect of
-- forcing strict evaluation, as the only way to extract the inner
-- value is to evaluate the 'Result' wrapper to WHNF.
instance Natify t => Natify (Value m -> t) where
  natify function = NativeFunction $ \(Result res) -> case res of
    Left err -> natify $ errorR err
    Right val -> natify $ function val

-- | Apply a native value as if it were a function, to zero or more arguments.
applyNative :: Native m -> [LazyValue m] -> LazyValue m
applyNative native [] = unwrapNative native
applyNative native (rval:rvals) = case native of
  NativeFunction func -> applyNative (func rval) rvals
  NativeValue v -> expectedFunction v

-- | Turn a 'Native' into a 'LazyValue'. This might rewrap it in a
-- 'VNative' constructor, if the object is not a 'NativeValue'.
unwrapNative :: Native m -> LazyValue m
unwrapNative (NativeValue rv) = rv
unwrapNative n = validR $ VNative n

-- | Create a value from a string.
strV :: Text -> Value m
strV = fromConstant . String

-- | Create a value from an integer.
intV :: Integer -> Value m
intV = fromConstant . Int

-- | Create a value from a bool.
boolV :: Bool -> Value m
boolV = fromConstant . Bool

-- | Create a null value.
nullV :: Value m
nullV = fromConstant Null

-- | Create an attribute set value.
attrsV :: [(Text, Value m)] -> Value m
attrsV = VAttrSet . mkEnv

-- | Create a list value.
listV :: [Value m] -> Value m
listV = VList . fromList . map validR

-- | Create a function value.
functionV :: Text -> Closure -> Value m
functionV param body = VFunction param body

-- | Create a native value.
nativeV :: Natify n => n -> Value m
nativeV = VNative . natify

-------------------------------------------------------------------------------
------------------------------- Value Types -----------------------------------
-------------------------------------------------------------------------------

-- | Runtime types of values.
data RuntimeType
  = RT_Null
  | RT_Bool
  | RT_Int
  | RT_String
  | RT_Function
  | RT_List
  | RT_AttrSet
  | RT_Path
  | RT_Error
  deriving (Show, Eq, Ord, Enum, Generic)

class HasRTType t where
  typeOf :: t -> RuntimeType

instance HasRTType Constant where
  typeOf (String _) = RT_String
  typeOf (Path _) = RT_Path
  typeOf (Int _) = RT_Int
  typeOf (Bool _) = RT_Bool
  typeOf Null = RT_Null

instance HasRTType t => HasRTType (Result m t) where
  typeOf (Result (Left _)) = RT_Error
  typeOf (Result (Right x)) = typeOf x

instance HasRTType (Native m) where
  typeOf (NativeValue v) = typeOf v
  typeOf (NativeFunction _) = RT_Function

instance HasRTType (Value m) where
  typeOf (VConstant constant) = typeOf constant
  typeOf (VAttrSet _) = RT_AttrSet
  typeOf (VList _) = RT_List
  typeOf (VFunction _ _) = RT_Function
  typeOf (VNative n) = typeOf n

hasType :: HasRTType t => RuntimeType -> t -> Bool
hasType type_ x = typeOf x == type_

typeToString :: RuntimeType -> Text
typeToString = T.toLower . T.replace "RT_" "" . tshow

-------------------------------------------------------------------------------
------------------------------ Environments -----------------------------------
-------------------------------------------------------------------------------

-- | An environment is conceptually just a name -> value mapping, but the
-- element type is actually a @LazyValue m@, so as to facilitate lazy
-- evaluation.
newtype Environment m = Environment {eEnv :: HashMap Text (LazyValue m)}
  deriving (Eq, Generic)

-- | We use a nix-esque syntax for showing an environment.
instance Show (Environment m) where
  show (Environment env) = "{" <> items <> "}" where
    showPair (n, v) = unpack n <> " = " <> show v
    items = intercalate "; " $ map showPair $ H.toList env

-- | We also use environments to represent attribute sets, since those
-- are lazily evaluated.
type AttrSet = Environment

-- | A closure is an unevaluated expression, with just an environment.
data Closure m = Closure (Environment m) Expression deriving (Eq, Generic)

instance Show (Closure m) where
  show (Closure env body) = "with " <> show env <> "; " <> show body

-- | Union two environments. Left-biased.
unionEnv :: Environment m -> Environment m -> Environment m
unionEnv (Environment e1) (Environment e2) = Environment (e1 `union` e2)

-- | Look up a name in an environment.
lookupEnv :: Text -> Environment m -> Maybe (LazyValue m)
lookupEnv name (Environment env) = H.lookup name env

-- | Insert a name/value into an environment.
insertEnv :: Text -> LazyValue m -> Environment m -> Environment m
insertEnv name res (Environment env) = Environment $ H.insert name res env

-- | Shorthand for creating an Environment from a list.
mkEnv :: [(Text, Value m)] -> Environment m
mkEnv = Environment . H.fromList . map (map validR)

-- | Empty environment.
emptyE :: Environment m
emptyE = mkEnv []

-- | Shorthand to create a closure from a list and an expression.
mkClosure :: [(Text, Value m)] -> Expression m -> Closure m
mkClosure env expr = Closure (mkEnv env) expr

-- | An empty closure.
emptyC :: Expression m -> Closure m
emptyC = mkClosure []

-------------------------------------------------------------------------------
--------------------------------- Errors --------------------------------------
-------------------------------------------------------------------------------

-- | The type of errors which can occur during evaluation.
data EvalError m
  = NameError Text (Environment m)
  -- ^ If we attempt to evaluate an undefined variable.
  | KeyError Text (AttrSet m)
  -- ^ If we attempt to grab a key which doesn't exist in a set.
  | IndexError Integer Int
  | TypeError (Set RuntimeType) RuntimeType
  -- ^ Type-related errors.
  | DivideByZero
  -- ^ For division.
  | CustomError Text
  -- ^ Some custom error message (e.g. from a user).
  | InfiniteRecursion
  -- ^ When we have some infinite loop going on.
  | AssertionError
  -- ^ When an assertion fails.
  | NotImplemented Text
  -- ^ For built-in functions/objects that we haven't implemented yet.
  | TailOfEmptyList
  deriving (Show, Eq, Typeable, Generic)

instance Exception (EvalError m)

-- | Wrap a value in a result.
validR :: Value m -> LazyValue m
validR = Result . Right

-- | Wrap an error in a result.
errorR :: EvalError m -> LazyValue m
errorR = Result . Left

-- | When expecting a single type.
expectedTheType :: RuntimeType -> RuntimeType -> EvalError m
expectedTheType t butGot = TypeError (S.singleton t) butGot

-- | Throw a type error when expecting a single type.
throwExpectedType :: HasRTType t => RuntimeType -> t -> LazyValue m
throwExpectedType expected val =
  errorR $ expectedTheType expected (typeOf val)

-- | When expecting a function.
expectedFunction :: HasRTType t => t -> LazyValue m
expectedFunction = throwExpectedType RT_Function

-- | When expecting a string.
expectedString :: HasRTType t => t -> LazyValue m
expectedString = throwExpectedType RT_String

-- | When expecting an integer.
expectedInt :: HasRTType t => t -> LazyValue m
expectedInt = throwExpectedType RT_Int

-- | When expecting a list.
expectedList :: HasRTType t => t -> LazyValue m
expectedList = throwExpectedType RT_List

-- | When expecting an attribute set.
expectedAttrs :: HasRTType t => t -> LazyValue m
expectedAttrs = throwExpectedType RT_AttrSet

-- | When expecting a boolean.
expectedBool :: HasRTType t => t -> LazyValue m
expectedBool = throwExpectedType RT_Bool

-- | When expecting one of a set of types..
expectedOneOf :: HasRTType t => [RuntimeType] -> t -> LazyValue m
expectedOneOf types val =
  errorR $ TypeError (S.fromList types) $ typeOf val

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Nix.Eval.Values where

import Nix.Common
import Nix.Eval.Expressions
import Nix.Eval.Constants
import qualified Data.HashMap.Strict as H
import qualified Data.Set as S
import qualified Data.Sequence as Seq

-------------------------------------------------------------------------------
--------------------------------- Values --------------------------------------
-------------------------------------------------------------------------------

-- | The type of runtime values. Is polymorphic over what type appears
-- in recursive variants such as environments and lists.
data Value' v
  = VConstant Constant
  -- ^ Constant values (isomorphic to constant expressions).
  | VAttrSet (AttrSet' v)
  -- ^ Attribute set values.
  | VList (Seq v)
  -- ^ List values.
  | VFunction Text (Closure' v)
  -- ^ Functions, with a parameter and a closure.
  | VNative (Native' v)
  -- ^ Native values, which can be either values or functions.
  deriving (Generic)

-- | Strict values are fully evaluated (at least conceptually);
-- internally they only contain other strict values.
newtype StrictValue = StrictValue (Value' StrictValue)
  deriving (Generic, Show, Eq)

instance Show v => Show (Value' v) where
  show (VConstant c) = "VConstant (" <> show c <> ")"
  show (VAttrSet s) = show s
  show (VList vs) = show vs
  show (VFunction param closure) = concat [ unpack param, " => ("
                                          , show closure, ")"]
  show (VNative (NativeValue rv)) = show rv
  show (VNative _) = "(native function)"

instance Eq v => Eq (Value' v) where
  VConstant c == VConstant c' = c == c'
  VAttrSet as == VAttrSet as' = as == as'
  VList vs == VList vs' = vs == vs'
  VFunction p1 e1 == VFunction p2 e2 = p1 == p2 && e1 == e2
  VNative (NativeValue nv) == VNative (NativeValue nv') = nv == nv'
  _ == _ = False

instance IsString (Value' v) where
  fromString = VConstant . fromString

instance FromConstant v => FromConstant (Value' v) where
  fromConstant = VConstant
 -- fromConstants = listV . map fromConstant
  fromConstantSet set = VAttrSet $ Environment $ map fromConstant set

-------------------------------------------------------------------------------
------------------------------ Lazy Values ------------------------------------
-------------------------------------------------------------------------------

-- | The result of evaluation: it might be an error. In Haskell, this
-- has the effect of creating lazy evaluation, as what is /inside/ the
-- result is not evaluated until inspected at some point.
newtype Result a = Result (Either EvalError a)
  deriving (Eq, Show, Functor, Applicative, Monad, Generic)

-- | Weak-head-normal-form values are strict at the top-level, but
-- internally may contain lazily evaluated values.
newtype Value = Value {
  unVal :: Value' LazyValue
  } deriving (Show, Eq)

-- | This is how we represent a lazily evaluated value: It's a
-- 'Result', which means it might be an error; but if it's not an
-- error it will be a value in WHNF.
type LazyValue = Result Value

instance FromConstant LazyValue where
  fromConstant = validR . fromConstant
  fromConstants = validR . fromConstants
  fromConstantSet = validR . fromConstantSet

instance FromConstant Value where
  fromConstant = Value . fromConstant

-- | Tests if a lazy value is an error (forces WHNF evaluation)
isError :: LazyValue -> Bool
isError (Result (Left _)) = True
isError _ = False

-- | Synonym for monadic bind, applying a function inside of a
-- 'Result', provided the 'Result' contains a 'Value'.
unwrapAndApply :: (Value -> LazyValue) -> LazyValue -> LazyValue
unwrapAndApply = (=<<)

-------------------------------------------------------------------------------
--------------------------- Deep Evaluation -----------------------------------
-------------------------------------------------------------------------------

-- | Most of the time we evaluate things lazily, but sometimes we want
-- to be able to evaluate strictly (esp. in the `deepSeq` function).
deeplyEval :: Value -> LazyValue
deeplyEval v@(Value val) = case val of
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

-- | Deeply evaluate a lazy value, while keeping it appearing as lazy.
deeplyEvalLazy :: LazyValue -> LazyValue
deeplyEvalLazy err@(Result (Left _)) = err
deeplyEvalLazy (Result (Right v)) = deeplyEval v

-------------------------------------------------------------------------------
------------------------------- Native Values ---------------------------------
-------------------------------------------------------------------------------


-- | An embedding of raw values. Lets us write functions in Haskell
-- which operate on Nix values, and expose these in Nix code.
data Native' v
  = NativeValue v
  -- ^ A terminal value
  | NativeFunction (v -> Native' v)
  -- ^ A function which lets us take the "next step" given a value.
  deriving (Generic)

-- | The most common use of natives is to encode lazy values.
type Native = Native' LazyValue

-- | Types which can be turned into 'Native' values.
class Natify t where
  natify :: t -> Native

-- | Of course, this applies to lazy values.
instance Natify LazyValue where
  natify = NativeValue

-- | This is where things get interesting: a 'Natify' instance for
-- functions on lazy values lets us embed any function on 'LazyValue's
-- as a 'Native' function. So for example, we can 'natify' a function
-- of type @'LazyValue' -> -- 'LazyValue'@, or @'LazyValue' ->
-- 'LazyValue' -> 'LazyValue'@, etc.
instance Natify t => Natify (LazyValue -> t) where
  natify function = NativeFunction $ \lval -> natify (function lval)

-- | We can 'natify' an arbitrary function on values (provided its
-- return type implements 'Natify'). However, it has the effect of
-- forcing strict evaluation, as the only way to extract the inner
-- value is to evaluate the 'Result' wrapper to WHNF.
instance Natify t => Natify (Value -> t) where
  natify function = NativeFunction $ \(Result res) -> case res of
    Left err -> NativeValue $ errorR err
    Right val -> natify $ function val

-- | Apply a native value as if it were a function, to zero or more arguments.
applyNative :: Native -> [LazyValue] -> LazyValue
applyNative native [] = unwrapNative native
applyNative native (rval:rvals) = case native of
  NativeFunction func -> applyNative (func rval) rvals
  NativeValue v -> expectedFunction v

-- | Turn a 'Native' into an 'LazyValue'. This might rewrap it in a
-- 'VNative' constructor, if the object is not a 'NativeValue'.
unwrapNative :: Native -> LazyValue
unwrapNative (NativeValue rv) = rv
unwrapNative n = validR $ Value $ VNative n

-- | Create a value from a string.
strV :: Text -> Value
strV = fromConstant . String

-- | Create a value from an integer.
intV :: Integer -> Value
intV = fromConstant . Int

-- | Create a value from a bool.
boolV :: Bool -> Value
boolV = fromConstant . Bool

-- | Create a null value.
nullV :: Value
nullV = fromConstant Null

-- | Create an attribute set value.
attrsV :: [(Text, Value)] -> Value
attrsV = Value . VAttrSet . mkEnv

-- | Create a list value.
listV :: [Value] -> Value
listV = Value . VList . Seq.fromList . map validR

-- | Create a function value.
functionV :: Text -> Closure -> Value
functionV param body = Value $ VFunction param body

-- | Create a native value.
nativeV :: Natify n => n -> Value
nativeV = Value . VNative . natify

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

instance NFData RuntimeType

class HasRTType t where
  typeOf' :: t -> Result RuntimeType
  typeOf :: t -> RuntimeType

instance HasRTType Constant where
  typeOf' (String _) = pure RT_String
  typeOf' (Path _) = pure RT_Path
  typeOf' (Int _) = pure RT_Int
  typeOf' (Bool _) = pure RT_Bool
  typeOf' Null = pure RT_Null

instance HasRTType t => HasRTType (Result t) where
  typeOf' res = do
    val <- res
    typeOf' val

instance HasRTType v => HasRTType (Native' v) where
  typeOf' (NativeValue v) = typeOf' v
  typeOf' (NativeFunction _) = pure RT_Function

instance HasRTType v => HasRTType (Value' v) where
  typeOf' (VConstant constant) = typeOf' constant
  typeOf' (VAttrSet _) = pure RT_AttrSet
  typeOf' (VList _) = pure RT_List
  typeOf' (VFunction _ _) = pure RT_Function
  typeOf' (VNative n) = typeOf' n

instance HasRTType Value where
  typeOf' (Value v) = typeOf' v

hasType :: HasRTType t => RuntimeType -> t -> Bool
hasType type_ x = typeOf x == type_

-------------------------------------------------------------------------------
------------------------------ Environments -----------------------------------
-------------------------------------------------------------------------------

-- | An environment is conceptually just a name -> value mapping, but the
-- element type is actually a @LazyValue@, so as to facilitate lazy
-- evaluation.
newtype Environment' v = Environment {eEnv :: HashMap Text v}
  deriving (Eq, Generic)

-- | We also use environments to represent attribute sets, since they
-- have the same behavior (in fact the `with` construct makes this
-- correspondence explicit).
type AttrSet' = Environment'

-- | Usually environments will contain LazyValues.
type Environment = Environment' LazyValue

-- | And so will attribute sets.
type AttrSet = AttrSet' LazyValue

-- | We use a nix-esque syntax for showing an environment.
instance Show v => Show (Environment' v) where
  show (Environment env) = "{" <> items <> "}" where
    showPair (n, v) = unpack n <> " = " <> show v
    items = intercalate "; " $ map showPair $ H.toList env

-- | A closure is an unevaluated expression, with just an environment.
data Closure' v = Closure (Environment' v) Expression deriving (Eq, Generic)

-- | The environment of most closures will be lazily evaluated.
type Closure = Closure' LazyValue

instance Show v => Show (Closure' v) where
  show (Closure env body) = "with " <> show env <> "; " <> show body

-- | Union two environments. Left-biased.
unionEnv :: Environment' v -> Environment' v -> Environment' v
unionEnv (Environment e1) (Environment e2) = Environment (e1 `union` e2)

-- | Look up a name in an environment.
lookupEnv :: Text -> Environment' v -> Maybe v
lookupEnv name (Environment env) = H.lookup name env

-- | Insert a name/value into an environment.
insertEnv :: Text -> v -> Environment' v -> Environment' v
insertEnv name res (Environment env) = Environment $ H.insert name res env

-- | Shorthand for creating an Environment from a list.
mkEnv :: [(Text, Value)] -> Environment
mkEnv = Environment . H.fromList . map (map validR)

-- | Empty environment.
emptyE :: Environment' v
emptyE = Environment mempty

-- | Shorthand to create a closure from a list and an expression.
mkClosure :: [(Text, Value)] -> Expression -> Closure
mkClosure env expr = Closure (mkEnv env) expr

-- | An empty closure.
emptyC :: Expression -> Closure' v
emptyC = Closure emptyE

-------------------------------------------------------------------------------
--------------------------------- Errors --------------------------------------
-------------------------------------------------------------------------------

-- | The type of errors which can occur during evaluation.
data EvalError' v
  = NameError Text (Environment' v)
  -- ^ If we attempt to evaluate an undefined variable.
  | KeyError Text (AttrSet' v)
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
  deriving (Show, Eq, Typeable, Generic)

type EvalError = EvalError' LazyValue

-- | Wrap a value in a result.
validR :: Value -> LazyValue
validR = Result . Right

-- | Wrap an error in a result.
errorR :: EvalError -> Result a
errorR = Result . Left

-- | When expecting a single type.
expectedTheType :: RuntimeType -> RuntimeType -> EvalError
expectedTheType t butGot = TypeError (S.singleton t) butGot

-- | Throw a type error when expecting a single type.
throwExpectedType :: HasRTType t => RuntimeType -> t -> Result a
throwExpectedType expected val =
  errorR $ expectedTheType expected (typeOf val)

-- | When expecting a function.
expectedFunction :: HasRTType t => t -> Result a
expectedFunction = throwExpectedType RT_Function

-- | When expecting a string.
expectedString :: HasRTType t => t -> Result a
expectedString = throwExpectedType RT_String

-- | When expecting an integer.
expectedInt :: HasRTType t => t -> Result a
expectedInt = throwExpectedType RT_Int

-- | When expecting a list.
expectedList :: HasRTType t => t -> Result a
expectedList = throwExpectedType RT_List

-- | When expecting an attribute set.
expectedAttrs :: HasRTType t => t -> Result a
expectedAttrs = throwExpectedType RT_AttrSet

-- | When expecting a boolean.
expectedBool :: HasRTType t => t -> Result a
expectedBool = throwExpectedType RT_Bool

-- | When expecting one of a set of types..
expectedOneOf :: HasRTType t => [RuntimeType] -> t -> Result a
expectedOneOf types val = do
  actualType <- typeOf' val
  errorR $ TypeError (S.fromList types) actualType

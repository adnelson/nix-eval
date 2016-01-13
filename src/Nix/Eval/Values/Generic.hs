{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
module Nix.Eval.Values.Generic where

import Nix.Common
import Nix.Eval.Expressions
import Nix.Eval.Constants
import qualified Data.HashMap.Strict as H
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import qualified Data.Text as T

-------------------------------------------------------------------------------
-- Values ---------------------------------------------------------------------
-------------------------------------------------------------------------------

-- | The type of runtime values. Is polymorphic over the computation
-- context type
data Value m
  = VConstant Constant
  -- ^ Constant values (isomorphic to constant expressions).
  | VAttrSet (AttrSet m)
  -- ^ Attribute set values.
  | VList (Seq (m (Value m)))
  -- ^ List values.
  | VFunction Text (Closure m)
  -- ^ Functions, with a parameter and a closure.
  | forall v. VNative (Native m v)
  -- ^ Native values, which can be either values or functions.

instance Extract m => Show (Value m) where
  show (VConstant c) = "VConstant (" <> show c <> ")"
  show (VAttrSet set) = show set
  show (VList vs) = show $ map extract vs
  show (VFunction param closure) = concat [ unpack param, " => ("
                                          , show closure, ")"]
  show (VNative (NativeValue v)) = show $ extract v
  show (VNative _) = "(native function)"

instance Extract m => Eq (Value m) where
  VConstant c == VConstant c' = c == c'
  VAttrSet as == VAttrSet as' = as == as'
  VList vs == VList vs' = map extract vs == map extract vs'
  VFunction p1 e1 == VFunction p2 e2 = p1 == p2 && e1 == e2
  VNative (NativeValue v) == VNative (NativeValue v') =
    extract v == extract v'
  _ == _ = False

instance IsString (Value m) where
  fromString = VConstant . fromString

instance Monad m => FromConstant (Value m) where
  fromConstant = VConstant
  fromConstants = VList . fromList . map (return . fromConstant)
  fromConstantSet set = VAttrSet $ Environment $
    map (return . fromConstant) set

instance Monad m => FromConstant (m (Value m)) where
  fromConstant = return . fromConstant
  fromConstants = return . fromConstants
  fromConstantSet = return . fromConstantSet

-- Convenience functions
-- | Shorthand for creating an Environment from a list.
mkEnv :: Monad m => [(Text, Value m)] -> Environment m
mkEnv = Environment . H.fromList . map (map pure)

-- | Shorthand to create a closure from a list and an expression.
mkClosure :: Monad m => [(Text, Value m)] -> Expression -> Closure m
mkClosure env expr = Closure (mkEnv env) expr

-- | Create a value from a string.
strV :: Monad m => Text -> Value m
strV = fromConstant . String

-- | Create a value from an integer.
intV :: Monad m => Integer -> Value m
intV = fromConstant . Int

-- | Create a value from a bool.
boolV :: Monad m => Bool -> Value m
boolV = fromConstant . Bool

-- | Create a null value.
nullV :: Monad m => Value m
nullV = fromConstant Null

-- | Create an attribute set value.
attrsV :: Monad m => [(Text, Value m)] -> Value m
attrsV = VAttrSet . mkEnv

-- | Create a list value.
listV :: Monad m => [Value m] -> Value m
listV = VList . fromList . map pure

-- | Create a function value.
functionV :: Monad m => Text -> Closure m -> Value m
functionV param closure = VFunction param closure

-- | Wrap a native into a value.
nativeV :: Monad m => Native m v -> Value m
nativeV = VNative

-------------------------------------------------------------------------------
-- Environments and Attribute Sets --------------------------------------------
-------------------------------------------------------------------------------

-- | An environment is conceptually just a name -> value mapping, but the
-- element type is parametric to allow usage of different kinds of values.
newtype Environment m = Environment {eEnv :: HashMap Text (m (Value m))}

instance Extract m => Eq (Environment m) where
  Environment e1 == Environment e2 = map extract e1 == map extract e2

-- | We also use environments to represent attribute sets, since they
-- have the same behavior (in fact the `with` construct makes this
-- correspondence explicit).
type AttrSet = Environment

-- | We can show an environment purely if the context implements extract.
instance (Extract ctx) => Show (Environment ctx) where
  show (Environment env) = "{" <> items <> "}" where
    showPair (n, v) = unpack n <> " = " <> show (extract v)
    items = intercalate "; " $ map showPair $ H.toList env

-- | A closure is an unevaluated expression, with just an environment.
data Closure m = Closure (Environment m) Expression
  deriving (Eq, Generic)

instance Extract m => Show (Closure m) where
  show (Closure env body) = "with " <> show env <> "; " <> show body

-- | Get the size of an environment.
envSize :: Environment m -> Int
envSize (Environment e) = H.size e

-- | Union two environments. Left-biased.
unionEnv :: Environment m -> Environment m -> Environment m
unionEnv (Environment e1) (Environment e2) = Environment (e1 `union` e2)

-- | Look up a name in an environment.
lookupEnv :: Text -> Environment m -> Maybe (m (Value m))
lookupEnv name (Environment env) = H.lookup name env

-- | Insert a name/value into an environment.
insertEnv :: Monad m => Text -> Value m -> Environment m -> Environment m
insertEnv name v (Environment env) = Environment $
  H.insert name (return v) env

-- | Insert a name/value into an environment, where the value is lazy.
insertEnvLazy :: Text -> m (Value m) -> Environment m -> Environment m
insertEnvLazy name v (Environment env) = Environment $ H.insert name v env

-- | Convert an environment to a list of (name, v).
envToList :: Environment m -> [(Text, m (Value m))]
envToList (Environment env) = H.toList env

-- | Get the set of keys in the environment.
envKeySet ::Environment m -> Set Text
envKeySet (Environment env) = S.fromList $ H.keys env

-- | An empty environment.
emptyE :: Environment m
emptyE = Environment mempty

-- | An empty closure.
emptyC :: Expression -> Closure v
emptyC = Closure emptyE

-------------------------------------------------------------------------------
-- Native Values --------------------------------------------------------------
-------------------------------------------------------------------------------

-- | An embedding of raw values. Lets us write functions in Haskell
-- which operate on Nix values, and expose these in Nix code.
data Native (m :: (* -> *)) :: * -> * where
  NativeValue :: m (Value m) -> Native m (Value m)
  -- ^ A terminal value (which has not necessarily been evaluated).
  NativeFunction :: (m (Value m) -> m (Native m v)) -> Native m (Value m -> v)
  -- ^ A function which lets us take the "next step" given a value.
  -- Either the argument or the result of this function might be
  -- failure, so we express that by having them be a monadic values.

-- | Apply a native value as if it were a function.
applyNative :: Native m (Value m -> t) -> m (Value m) -> m (Native m t)
applyNative (NativeFunction func) arg = func arg

-- | Apply a native value as if it were a function, to two arguments.
applyNative2 :: Monad m =>
             Native m (Value m -> Value m -> t) ->
             m (Value m) -> m (Value m) -> m (Native m t)
applyNative2 (NativeFunction func) x y = do
  NativeFunction newFunc <- func x
  newFunc y

-- | Turn a 'Native' into a monadic 'Value'. This is useful because
-- objects built with 'NativeValue' are isomorphic to 'Value's.
-- Unwrapping them here means the only 'Native' values that we need to
-- keep around are 'NativeFunction's.
unwrapNative :: Monad m => Native m v -> m (Value m)
unwrapNative (NativeValue v) = v
unwrapNative n = return $ VNative n

-------------------------------------------------------------------------------
-- Value Types ----------------------------------------------------------------
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
  deriving (Show, Eq, Ord, Enum, Generic)

instance NFData RuntimeType

-- | Things that have runtime types. Those types are discovered
-- through some computation context @m@ (which must be a monad, since
-- the computation could fail).
class MonadError EvalError m => HasRTType t m where
  typeOf :: t -> m RuntimeType

-- | String representation of runtime types.
typeToString :: RuntimeType -> Text
typeToString = T.toLower . T.replace "RT_" "" . tshow


-- | Constants have a runtime type which can be determined in O(1).
typeOfConstant :: Constant -> RuntimeType
typeOfConstant (String _) = RT_String
typeOfConstant (Path _) = RT_Path
typeOfConstant (Int _) = RT_Int
typeOfConstant (Bool _) = RT_Bool
typeOfConstant Null = RT_Null

-- | If some type @t@ has a runtime type but might fail with @EvalError v@,
-- then a 'Eval' returning @t@ also a runtime type. If the result fails to
-- evaluate then we throw an @EvalError v@; otherwise we return t's type.
instance (MonadError EvalError m, HasRTType t m) => HasRTType (m t) m where
  typeOf res = res >>= typeOf

instance (MonadError EvalError m) => HasRTType (Value m) m where
  typeOf (VConstant constant) = pure $ typeOfConstant constant
  typeOf (VAttrSet _) = pure RT_AttrSet
  typeOf (VList _) = pure RT_List
  typeOf (VFunction _ _) = pure RT_Function
  typeOf (VNative (NativeValue v)) = typeOf v
  typeOf (VNative (NativeFunction _)) = pure RT_Function

hasType :: HasRTType t m => RuntimeType -> t -> m Bool
hasType expectedType obj = do
  actualType <- typeOf obj
  return $ actualType == expectedType

-------------------------------------------------------------------------------
-- Errors ---------------------------------------------------------------------
-------------------------------------------------------------------------------

-- | The type of errors which can occur during evaluation.
data EvalError
  = NameError Text (Set Text)
  -- ^ If we attempt to evaluate an undefined variable.
  | KeyError Text (Set Text)
  -- ^ If we attempt to grab a key which doesn't exist in a set.
  | IndexError Integer Int
  -- ^ If we try to index into a list which is too short.
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
  -- ^ For native functions we haven't implemented yet.
  deriving (Show, Eq, Typeable, Generic)

-- | When expecting a single type.
expectedTheType :: RuntimeType -> RuntimeType -> EvalError
expectedTheType t butGot = TypeError (S.singleton t) butGot

-- | Throw a type error when expecting a single type.
throwExpectedType :: HasRTType t m => RuntimeType -> t -> m a
throwExpectedType expected val = do
  actual <- typeOf val
  throwError $ expectedTheType expected actual

-- | When expecting a function.
expectedFunction :: HasRTType t m => t -> m a
expectedFunction = throwExpectedType RT_Function

-- | When expecting a string.
expectedString :: HasRTType t m => t -> m a
expectedString = throwExpectedType RT_String

-- | When expecting an integer.
expectedInt :: HasRTType t m => t -> m a
expectedInt = throwExpectedType RT_Int

-- | When expecting a list.
expectedList :: HasRTType t m => t -> m a
expectedList = throwExpectedType RT_List

-- | When expecting an attribute set.
expectedAttrs :: HasRTType t m => t -> m a
expectedAttrs = throwExpectedType RT_AttrSet

-- | When expecting a boolean.
expectedBool :: HasRTType t m => t -> m a
expectedBool = throwExpectedType RT_Bool

-- | When expecting one of a set of types..
expectedOneOf :: HasRTType t m => [RuntimeType] -> t -> m a
expectedOneOf types val = do
  actualType <- typeOf val
  throwError $ TypeError (S.fromList types) actualType

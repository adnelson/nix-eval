{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
module Nix.Eval.Values.Generic where

import Nix.Common
import Nix.Eval.Expressions
import Nix.Eval.Constants
import qualified Data.HashMap.Strict as H
import qualified Data.Set as S
import qualified Data.Sequence as Seq

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
  | VList (Seq (MValue m))
  -- ^ List values.
  | VFunction Text (Closure m)
  -- ^ Functions, with a parameter and a closure.
  | forall v. VNative (Native m v)
  -- ^ Native values, which can be either values or functions.

-- | A 'Value' wrapped in its monadic context.
type MValue m = m (Value m)

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

-------------------------------------------------------------------------------
-- Environments and Attribute Sets --------------------------------------------
-------------------------------------------------------------------------------

-- | An environment is conceptually just a name -> value mapping, but the
-- element type is parametric to allow usage of different kinds of values.
newtype Environment m = Environment {eEnv :: HashMap Text (MValue m)}

instance Extract m => Eq (Environment m) where
  Environment e1 == Environment e2 = map extract e1 == map extract e2

-- | We also use environments to represent attribute sets, since they
-- have the same behavior (in fact the `with` construct makes this
-- correspondence explicit).
type AttrSet = Environment

-- | We use a nix-esque syntax for showing an environment.
instance (Extract m) => Show (Environment m) where
  show (Environment env) = "{" <> items <> "}" where
    showPair (n, v) = unpack n <> " = " <> show (extract v)
    items = intercalate "; " $ map showPair $ H.toList env

-- | A closure is an unevaluated expression, with just an environment.
data Closure m = Closure (Environment m) Expression
  deriving (Eq, Generic)

instance Extract m => Show (Closure m) where
  show (Closure env body) = "with " <> show env <> "; " <> show body

-- | Union two environments. Left-biased.
unionEnv :: Environment m -> Environment m -> Environment m
unionEnv (Environment e1) (Environment e2) = Environment (e1 `union` e2)

-- | Look up a name in an environment.
lookupEnv :: Text -> Environment m -> Maybe (MValue m)
lookupEnv name (Environment env) = H.lookup name env

-- | Insert a name/value into an environment.
insertEnv :: Monad m => Text -> Value m -> Environment m -> Environment m
insertEnv name v (Environment env) = Environment $
  H.insert name (return v) env

-- | Convert an environment to a list of (name, v).
envToList :: Environment m -> [(Text, MValue m)]
envToList (Environment env) = H.toList env

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
  NativeValue :: MValue m -> Native m (MValue m)
  -- ^ A terminal value (which has not necessarily been evaluated).
  NativeFunction :: (m v1 -> m (Native m v2)) -> Native m (v1 -> v2)
  -- ^ A function which lets us take the "next step" given a value.
  -- Either the argument or the result of this function might be
  -- failure, so we express that by having them be a monadic values.

-- | Apply a native value as if it were a function.
applyNative :: Native m (a -> b) -> m a -> m (Native m b)
applyNative (NativeFunction func) arg = func arg

unwrapNative :: Monad m => Native m v -> MValue m
unwrapNative (NativeValue v) = v
unwrapNative n = return $ VNative n

-- -- | The relation @Natify t m@ states that given a value of type @t@,
-- -- I can produce a value of type @Native v@ in the context @m@, where
-- -- @v@ is the result of evaluating @t@.
-- class Natify t m where
--   type EvalResult t :: *
--   natify :: t -> Native m (EvalResult t)

-- -- | This states the (obvious) fact that given a native, I can produce
-- -- another native value of the same type.
-- instance Natify (Native m v) m where
--   type EvalResult (Native m v) = v
-- --  type EvalMonad (Native m v) = m
--   natify = id

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
  deriving (Show, Eq, Ord, Enum, Generic)

instance NFData RuntimeType

class Monad m => Context m where
  errorR :: EvalError v -> m a

-- | Things that have runtime types. Those types are discovered
-- through some computation context @m@ (which must be a monad, since
-- the computation could fail).
class Context m => HasRTType t m where
  typeOf :: t -> m RuntimeType

-- | Constants have a runtime type and will never fail; they can be used with
-- any context.
instance Context m => HasRTType Constant m where
  typeOf (String _) = pure RT_String
  typeOf (Path _) = pure RT_Path
  typeOf (Int _) = pure RT_Int
  typeOf (Bool _) = pure RT_Bool
  typeOf Null = pure RT_Null

-- | If some type @t@ has a runtime type but might fail with @EvalError v@,
-- then a 'Eval' returning @t@ also a runtime type. If the result fails to
-- evaluate then we throw an @EvalError v@; otherwise we return t's type.
instance (Context m, HasRTType t m) => HasRTType (m t) m where
  typeOf res = res >>= typeOf

instance (Context m) => HasRTType (Value m) m where
  typeOf (VConstant constant) = typeOf constant
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
  deriving (Show, Eq, Typeable, Generic)

-- | When expecting a single type.
expectedTheType :: RuntimeType -> RuntimeType -> EvalError m
expectedTheType t butGot = TypeError (S.singleton t) butGot

-- | Throw a type error when expecting a single type.
throwExpectedType :: HasRTType t m => RuntimeType -> t -> m a
throwExpectedType expected val = do
  actual <- typeOf val
  errorR $ expectedTheType expected actual

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
  errorR $ TypeError (S.fromList types) actualType

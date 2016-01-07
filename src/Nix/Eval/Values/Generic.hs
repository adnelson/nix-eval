{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
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
  deriving (Generic, Functor)

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
  fromConstants = VList . fromList . map fromConstant
  fromConstantSet set = VAttrSet $ Environment $ map fromConstant set

-- | The result of evaluation: it might be an error. In Haskell, this
-- has the effect of creating lazy evaluation, as what is /inside/ the
-- result is not evaluated until inspected at some point.
newtype Result' v a = Result (Either (EvalError' v) a)
  deriving (Eq, Show, Functor, Applicative, Monad, Generic)

-------------------------------------------------------------------------------
-- Environments and Attribute Sets --------------------------------------------
-------------------------------------------------------------------------------

-- | An environment is conceptually just a name -> value mapping, but the
-- element type is parametric to allow usage of different kinds of values.
newtype Environment' v = Environment {eEnv :: HashMap Text v}
  deriving (Eq, Functor, Generic)

-- | We also use environments to represent attribute sets, since they
-- have the same behavior (in fact the `with` construct makes this
-- correspondence explicit).
type AttrSet' = Environment'

-- | We use a nix-esque syntax for showing an environment.
instance Show v => Show (Environment' v) where
  show (Environment env) = "{" <> items <> "}" where
    showPair (n, v) = unpack n <> " = " <> show v
    items = intercalate "; " $ map showPair $ H.toList env

-- | A closure is an unevaluated expression, with just an environment.
data Closure' v = Closure (Environment' v) Expression
  deriving (Eq, Functor, Generic)

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

-- | Convert an environment to a list of (name, v).
envToList :: Environment' v -> [(Text, v)]
envToList (Environment env) = H.toList env

-- | An empty environment.
emptyE :: Environment' v
emptyE = Environment mempty

-- | An empty closure.
emptyC :: Expression -> Closure' v
emptyC = Closure emptyE

-------------------------------------------------------------------------------
-- Native Values --------------------------------------------------------------
-------------------------------------------------------------------------------

-- | An embedding of raw values. Lets us write functions in Haskell
-- which operate on Nix values, and expose these in Nix code.
data Native' v
  = NativeValue v
  -- ^ A terminal value
  | NativeFunction (v -> Result' v (Native' v))
  -- ^ A function which lets us take the "next step" given a value.
  deriving (Generic)

-- | This is a kind of sketchy functor instance because of the partiality.
instance Functor Native' where
  fmap f (NativeValue v) = NativeValue $ f v
  fmap f (NativeFunction nf) = error "Can't map over a native function"

-- | Types which can be turned into 'Native' values.
class Natify t where
  natify :: t -> Native' v

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

-- | Things that have runtime types. The @v@ here refers to the type of
-- values found in any 'EvalError's that might be returned if the
-- computation fails.
class HasRTType t v where
  typeOf :: t -> Result' v RuntimeType

-- | Constants have a runtime type and will never fail; they can be used with
-- any 'Result' type.
instance HasRTType Constant v where
  typeOf (String _) = pure RT_String
  typeOf (Path _) = pure RT_Path
  typeOf (Int _) = pure RT_Int
  typeOf (Bool _) = pure RT_Bool
  typeOf Null = pure RT_Null

-- | If some type @t@ has a runtime type but might fail with @EvalError' v@,
-- then a 'Result' returning @t@ also a runtime type. If the result fails to
-- evaluate then we throw an @EvalError v@; otherwise we return t's type.
instance HasRTType t v => HasRTType (Result' v t) v where
  typeOf res = res >>= typeOf

instance HasRTType v1 v2 => HasRTType (Native' v1) v2 where
  typeOf (NativeValue v) = typeOf v
  typeOf (NativeFunction _) = pure RT_Function

instance HasRTType v1 v2 => HasRTType (Value' v1) v2 where
  typeOf (VConstant constant) = typeOf constant
  typeOf (VAttrSet _) = pure RT_AttrSet
  typeOf (VList _) = pure RT_List
  typeOf (VFunction _ _) = pure RT_Function
  typeOf (VNative n) = typeOf n

hasType :: HasRTType t v => RuntimeType -> t -> Result' v Bool
hasType expectedType obj = do
  actualType <- typeOf obj
  return $ actualType == expectedType

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

-- | Wrap an error in a result.
errorR :: EvalError' v -> Result' v a
errorR = Result . Left

-- | When expecting a single type.
expectedTheType :: RuntimeType -> RuntimeType -> EvalError' v
expectedTheType t butGot = TypeError (S.singleton t) butGot

-- | Throw a type error when expecting a single type.
throwExpectedType :: HasRTType t v => RuntimeType -> t -> Result' v a
throwExpectedType expected val = do
  actual <- typeOf val
  errorR $ expectedTheType expected actual

-- | When expecting a function.
expectedFunction :: HasRTType t v => t -> Result' v a
expectedFunction = throwExpectedType RT_Function

-- | When expecting a string.
expectedString :: HasRTType t v => t -> Result' v a
expectedString = throwExpectedType RT_String

-- | When expecting an integer.
expectedInt :: HasRTType t v => t -> Result' v a
expectedInt = throwExpectedType RT_Int

-- | When expecting a list.
expectedList :: HasRTType t v => t -> Result' v a
expectedList = throwExpectedType RT_List

-- | When expecting an attribute set.
expectedAttrs :: HasRTType t v => t -> Result' v a
expectedAttrs = throwExpectedType RT_AttrSet

-- | When expecting a boolean.
expectedBool :: HasRTType t v => t -> Result' v a
expectedBool = throwExpectedType RT_Bool

-- | When expecting one of a set of types..
expectedOneOf :: HasRTType t v => [RuntimeType] -> t -> Result' v a
expectedOneOf types val = do
  actualType <- typeOf val
  errorR $ TypeError (S.fromList types) actualType

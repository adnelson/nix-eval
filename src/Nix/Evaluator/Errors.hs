-- | Describes errors that can be encountered during evaluation.
module Nix.Evaluator.Errors where

import Nix.Common
import Nix.Values.Generic
import Nix.Evaluator.RuntimeTypes
import qualified Data.Set as S

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
  | AbortExecution Text
  -- ^ When this error is raised, execution will be aborted without
  -- the ability to recover.
  | InfiniteRecursion
  -- ^ When we have some infinite loop going on.
  | AssertionError
  -- ^ When an assertion fails.
  | EmptyList
  -- ^ When attempting to get the head or tail of an empty list.
  | MissingArguments [Text]
  -- ^ When not enough arguments are passed to a function.
  | ExtraArguments [Text]
  -- ^ When too many arguments are passed to a function.
  | DuplicateKey Text
  -- ^ Raised when the same key is assigned twice in an attribute set.
  | NotImplemented Text
  -- ^ For native functions we haven't implemented yet.
  deriving (Show, Eq, Typeable, Generic)

-- | Things that have runtime types. Those types are discovered
-- through some computation context @m@ (which must be a monad, since
-- the computation could fail).
class MonadError EvalError m => HasRTType t m where
  typeOf :: t -> m RuntimeType

-- | If some type @t@ has a runtime type but might fail with @EvalError v@,
-- then a 'Eval' returning @t@ also a runtime type. If the result fails to
-- evaluate then we throw an @EvalError v@; otherwise we return t's type.
instance (MonadError EvalError m, HasRTType t m) => HasRTType (m t) m where
  typeOf res = res >>= typeOf

instance (MonadError EvalError m) => HasRTType (Value m) m where
  typeOf (VConstant constant) = pure $ typeOfConstant constant
  typeOf (VAttrSet _) = pure RT_Set
  typeOf (VList _) = pure RT_List
  typeOf (VFunction _ _) = pure RT_Lambda
  typeOf (VNative (NativeValue v)) = typeOf v
  typeOf (VNative (NativeFunction _)) = pure RT_Lambda

-- | Return whether the given object has the given runtime type.
hasType :: HasRTType t m => RuntimeType -> t -> m Bool
hasType expectedType obj = do
  actualType <- typeOf obj
  return $ actualType == expectedType

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
expectedFunction = throwExpectedType RT_Lambda

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
expectedAttrs = throwExpectedType RT_Set

-- | When expecting a boolean.
expectedBool :: HasRTType t m => t -> m a
expectedBool = throwExpectedType RT_Bool

-- | When expecting one of a set of types..
expectedOneOf :: HasRTType t m => [RuntimeType] -> t -> m a
expectedOneOf types val = do
  actualType <- typeOf val
  throwError $ TypeError (S.fromList types) actualType

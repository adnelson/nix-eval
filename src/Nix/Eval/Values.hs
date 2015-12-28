{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
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

-- | The result of evaluation: it might be an error.
newtype Result a = Result (Either EvalError a)
  deriving (Show, Eq, Functor, Applicative, Monad)

-- | A commonly-used type; the result of an evaluation.
type RValue = Result Value

-- | Builtin functions concaptually take a value, but it's actually a
-- (possibly unevaluated) Result. They are functions which can either
-- return an error or a new Result.
data NativeFunc1 = Func1 Text (RValue -> RValue)

instance Show NativeFunc1 where
  show (Func1 name _) = "Native function " <> show name

instance Eq NativeFunc1 where
  Func1 name _ == Func1 name' _ = name == name'

-- | Same as @NativeFunc1@, but for arity 2.
data NativeFunc2 = Func2 Text (RValue -> RValue -> RValue)

-- | Things which can be "called" with a lazy argument. This includes
-- functions defined in nix, as well as builtins. We require they be
-- Show and Eq instances, for testing/debugging purposes. We leave the
-- evaluator function to be passed in as an argument, so that we don't
-- have to put the evaluator code in this module.
class (Show func, Eq func) => Callable func where
  call :: (Environment -> Expression -> RValue)
       -> (func -> RValue -> RValue)

-- | The type of runtime values.
data Value
  = VConstant Constant
  -- ^ Constant values (isomorphic to constant expressions).
  | VAttrSet AttrSet
  -- ^ Attribute set values.
  | VList (Seq Value)
  -- ^ List values.
  | forall func. Callable func => VCallable func
  | VFunction Function
  -- ^ Functions. These are more specific than generic callables
  -- because we want to be able to test them for equality (in our
  -- test suites)
  | VBuiltin Text BuiltinFunc
  -- ^ Built-in unary functions.
  | VBuiltin2 Text BuiltinFunc2
  -- ^ Built-in binary functions.

instance Show Value where
  show (VConstant c) = show c
  show (VAttrSet s) = show s
  show (VList vs) = show vs
  show (VCallable func) = show func
  show (VFunction func) = show func
  show (VBuiltin name _) = "BUILTIN(" <> unpack name <> ")"
  show (VBuiltin2 name _) = "BUILTIN2(" <> unpack name <> ")"

instance Eq Value where
  VConstant c == VConstant c' = c == c'
  VAttrSet as == VAttrSet as' = as == as'
  VList vs == VList vs' = vs == vs'
  VFunction f1 == VFunction f2 = f1 == f2
  VBuiltin name _ == VBuiltin name' _ = name == name'
  VBuiltin2 name _ == VBuiltin2 name' _ = name == name'
  _ == _ = False

instance IsString Value where
  fromString = VConstant . fromString

-- | Create a value from a constant.
vConstant :: Constant -> Value
vConstant = VConstant

-- | Create a value from a string.
strV :: Text -> Value
strV = vConstant . String

-- | Create a value from an integer.
intV :: Integer -> Value
intV = vConstant . Int

-- | Create a value from a bool.
boolV :: Bool -> Value
boolV = vConstant . Bool

-- | Create an attribute set value.
attrsV :: [(Text, Value)] -> Value
attrsV = VAttrSet . mkEnv

-- | Create a list value.
listV :: [Value] -> Value
listV = VList . Seq.fromList

-- | Create a function value.
functionV :: Text -> Closure -> Value
functionV param body = VFunction $ Function param body

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
  deriving (Show, Eq, Ord)

typeOfConstant :: Constant -> RuntimeType
typeOfConstant (String _) = RT_String
typeOfConstant (Path _) = RT_Path
typeOfConstant (Int _) = RT_Int
typeOfConstant (Bool _) = RT_Bool
typeOfConstant Null = RT_Null

typeOfValue :: Value -> RuntimeType
typeOfValue (VConstant constant) = typeOfConstant constant
typeOfValue (VAttrSet _) = RT_AttrSet
typeOfValue (VList _) = RT_List
typeOfValue (VCallable _) = RT_Function
typeOfValue (VFunction _) = RT_Function
typeOfValue (VBuiltin _ _) = RT_Function
typeOfValue (VBuiltin2 _ _) = RT_Function


-------------------------------------------------------------------------------
------------------------------ Environments -----------------------------------
-------------------------------------------------------------------------------

-- | An environment is conceptually just a name -> value mapping, but the
-- element type is actually a @RValue@, so as to facilitate lazy
-- evaluation.
newtype Environment = Environment {eEnv :: HashMap Text (RValue)}
  deriving (Eq)

-- | We also use environments to represent attribute sets, since those
-- are lazily evaluated.
type AttrSet = Environment

-- | We use a nix-esque syntax for showing an environment.
instance Show Environment where
  show (Environment env) = "{" <> items <> "}" where
    showPair (n, v) = unpack n <> " = " <> show v
    items = intercalate "; " $ map showPair $ H.toList env

-- | A closure is an unevaluated expression, with just an environment.
data Closure = Closure Environment Expression deriving (Eq)

-- | An evaluated function has a parameter name and a closure.
data Function = Function Text Closure deriving (Eq)

instance Callable Function where
  call eval (Function param (Closure cEnv body)) arg =
    eval (insertEnv param arg cEnv) body

instance Show Function where
  show (Function param closure) = concat [ unpack param, " => ("
                                         , show closure, ")"]


instance Show Closure where
  show (Closure env body) = "with " <> show env <> "; " <> show body

-- | Union two environments. Left-biased.
unionEnv :: Environment -> Environment -> Environment
unionEnv (Environment e1) (Environment e2) = Environment (e1 `union` e2)

-- | Look up a name in an environment.
lookupEnv :: Text -> Environment -> Maybe (RValue)
lookupEnv name (Environment env) = H.lookup name env

-- | Insert a name/value into an environment.
insertEnv :: Text -> RValue -> Environment -> Environment
insertEnv name res (Environment env) = Environment $ H.insert name res env

-- | Shorthand for creating an Environment from a list.
mkEnv :: [(Text, Value)] -> Environment
mkEnv = Environment . H.fromList . map (map $ Result . Right)

-- | Empty environment.
emptyE :: Environment
emptyE = mkEnv []

-- | Shorthand to create a closure from a list and an expression.
mkClosure :: [(Text, Value)] -> Expression -> Closure
mkClosure env expr = Closure (mkEnv env) expr

-- | An empty closure.
emptyC :: Expression -> Closure
emptyC = mkClosure []

-------------------------------------------------------------------------------
--------------------------------- Errors --------------------------------------
-------------------------------------------------------------------------------

-- | The type of errors which can occur during evaluation.
data EvalError
  = NameError Text Environment
  -- ^ If we attempt to evaluate an undefined variable.
  | KeyError Text AttrSet
  -- ^ If we attempt to grab a key which doesn't exist in a set.
  | TypeError (Set RuntimeType) RuntimeType
  -- ^ Type-related errors.
  | DivideByZero
  -- ^ For division.
  | CustomError Text
  -- ^ Some custom error message (e.g. from a user).
  deriving (Show, Eq, Typeable)

instance Exception EvalError

isValid :: Result a -> Bool
isValid (Result (Right _)) = True
isValid _ = False

isError :: Result a -> Bool
isError (Result (Left _)) = True
isError _ = False

validR :: a -> Result a
validR = Result . Right

errorR :: EvalError -> Result a
errorR = Result . Left

throwEvalError :: EvalError -> IO a
throwEvalError = throwIO

throwPure :: EvalError -> Result a
throwPure = Result . Left

expectedTheType :: RuntimeType -> RuntimeType -> EvalError
expectedTheType t butGot = TypeError (S.singleton t) butGot

throwExpectedType :: RuntimeType -> Value -> Result a
throwExpectedType expected val =
  throwPure $ expectedTheType expected (typeOfValue val)

expectedFunction :: Value -> Result a
expectedFunction = throwExpectedType RT_Function

expectedString :: Value -> Result a
expectedString = throwExpectedType RT_String

expectedInt :: Value -> Result a
expectedInt = throwExpectedType RT_Int

expectedList :: Value -> Result a
expectedList = throwExpectedType RT_List

expectedAttrs :: Value -> Result a
expectedAttrs = throwExpectedType RT_AttrSet

expectedBool :: Value -> Result a
expectedBool = throwExpectedType RT_Bool

expectedOneOf :: [RuntimeType] -> Value -> Result a
expectedOneOf types val =
  throwPure $ TypeError (S.fromList types) $ typeOfValue val

throwCustom :: Text -> Result a
throwCustom = throwPure . CustomError

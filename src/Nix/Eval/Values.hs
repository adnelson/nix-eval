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

class FunctionLike t where
  callAsFunction :: t -> Result Value -> Result Value

data SomeFunction = forall t. FunctionLike t => SomeFunction t

-- | Builtin functions concaptually take a value, but it's actually a
-- (possibly unevaluated) Result. They are functions which can either
-- return an error or a new Result.
type BuiltinFunc = Result Value -> Result Value

-- | Same as @BuiltinFunc@, but for arity 2.
type BuiltinFunc2 = Result Value -> BuiltinFunc

data Value
  = VConstant Constant
  -- ^ Constant values (isomorphic to constant expressions).
  | VAttrSet Environment
  -- ^ Attribute set values.
  | VList (Seq Value)
  -- ^ List values.
  | VFunction Text Closure
  -- ^ Functions, which have an argument name and a closure.
  | VBuiltin Text BuiltinFunc
  -- ^ Built-in unary functions.
  | VBuiltin2 Text BuiltinFunc2
  -- ^ Built-in binary functions.

instance Show Value where
  show (VConstant c) = show c
  show (VAttrSet s) = show s
  show (VList vs) = show vs
  show (VFunction param closure) = concat [ unpack param, " => ("
                                          , show closure, ")"]
  show (VBuiltin name _) = "BUILTIN(" <> unpack name <> ")"
  show (VBuiltin2 name _) = "BUILTIN2(" <> unpack name <> ")"

instance Eq Value where
  VConstant c == VConstant c' = c == c'
  VAttrSet as == VAttrSet as' = as == as'
  VList vs == VList vs' = vs == vs'
  VFunction p b == VFunction p' b' = p == p' && b == b'
  VBuiltin name _ == VBuiltin name' _ = name == name'
  VBuiltin2 name _ == VBuiltin2 name' _ = name == name'
  _ == _ = False

instance IsString Value where
  fromString = VConstant . fromString

-- | Create a value from a string.
strV :: Text -> Value
strV = vConstant . String

-- | Create a value from an integer.
intV :: Integer -> Value
intV = vConstant . Int

-- | Create a value from a bool.
boolV :: Bool -> Value
boolV = vConstant . Bool

attrsV :: [(Text, Value)] -> Value
attrsV = VAttrSet . mkEnv

listV :: [Value] -> Value
listV = VList . Seq.fromList

vConstant :: Constant -> Value
vConstant = VConstant

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
typeOfValue (VFunction _ _) = RT_Function
typeOfValue (VBuiltin _ _) = RT_Function
typeOfValue (VBuiltin2 _ _) = RT_Function


-------------------------------------------------------------------------------
------------------------------ Environments -----------------------------------
-------------------------------------------------------------------------------

-- | An environment is conceptually just a name -> value mapping, but the
-- element type is actually a @Result Value@, so as to facilitate lazy
-- evaluation.
newtype Environment = Environment {eEnv :: HashMap Text (Result Value)}
  deriving (Eq)

instance Show Environment where
  show (Environment env) = "{" <> items <> "}" where
    showPair (n, v) = unpack n <> " = " <> show v
    items = intercalate ", " $ map showPair $ H.toList env

-- | A closure is an unevaluated expression, with just an environment.
data Closure = Closure Environment Expression deriving (Eq)

instance Show Closure where
  show (Closure env body) = "with " <> show env <> "; " <> show body

-- | Union two environments. Left-biased.
unionEnv :: Environment -> Environment -> Environment
unionEnv (Environment e1) (Environment e2) = Environment (e1 `union` e2)

lookupEnv :: Text -> Environment -> Maybe (Result Value)
lookupEnv name (Environment env) = H.lookup name env

insertEnv :: Text -> Result Value -> Environment -> Environment
insertEnv name res (Environment env) = Environment $ H.insert name res env

singleEnv :: Text -> Value -> Environment
singleEnv name val = Environment $ H.singleton name (Result $ Right val)

mkEnv :: [(Text, Value)] -> Environment
mkEnv = Environment . H.fromList . map (map $ Result . Right)

emptyE :: Environment
emptyE = mkEnv []

mkClosure :: [(Text, Value)] -> Expression -> Closure
mkClosure env expr = Closure (mkEnv env) expr

emptyC :: Expression -> Closure
emptyC = mkClosure []

-------------------------------------------------------------------------------
--------------------------------- Errors --------------------------------------
-------------------------------------------------------------------------------

data EvalError
  = NameError Text Environment
  -- ^ If we attempt to evaluate an undefined variable.
  | KeyError Text Environment
  -- ^ If we attempt to grab a key which doesn't exist in a set.
  | TypeError (Set RuntimeType) RuntimeType
  -- ^ Type-related errors.
  | CustomError Text
  -- ^ Some custom error message.
  deriving (Show, Eq, Typeable)

instance Exception EvalError

newtype Result a = Result (Either EvalError a)
  deriving (Show, Eq, Functor, Applicative, Monad)

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

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
module Nix.Eval.Values where

import Nix.Common
import Nix.Eval.Expressions
import Nix.Eval.Constants
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H
import qualified Data.Set as S

-------------------------------------------------------------------------------
--------------------------------- Values --------------------------------------
-------------------------------------------------------------------------------

data Value
  = VConstant Constant
  -- ^ Constant values (isomorphic to constant expressions).
  | VAttrSet (HashMap Text Value)
  -- ^ Attribute set values.
  | VList [Value]
  -- ^ List values.
  | VFunction Text Closure
  -- ^ Functions, which have an argument name and a closure.
  | VFunc (Value -> Either Text Value)
  | VBuiltin Text (Value -> Result Value)
  -- ^ Built-in functions.

instance Show Value where
  show (VConstant c) = show c
  show (VAttrSet s) = do
    let pairs = H.toList s
        showPair (k, v) = unpack k <> " = " <> show v
    "{" <> intercalate "; " (map showPair pairs) <> "}"
  show (VList vs) = show vs
  show (VFunction param closure) = concat [ unpack param, " => ("
                                          , show closure, ")"]
  show (VBuiltin name _) = "BUILTIN(" <> unpack name <> ")"

-- | Create a value from a string.
strV :: Text -> Value
strV = vConstant . String

-- | Create a value from an integer.
intV :: Int -> Value
intV = vConstant . Int

-- | Create a value from a bool.
boolV :: Bool -> Value
boolV = vConstant . Bool

getEnvF :: Environment -> HashMap Text Value
getEnvF (Environment env) = env

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


-------------------------------------------------------------------------------
------------------------------ Environments -----------------------------------
-------------------------------------------------------------------------------

-- | An environment is just a name -> value mapping.
newtype Environment = Environment {
  eEnv :: HashMap Text Value
  } deriving (Show)

-- | A closure is an unevaluated expression, with just an environment.
data Closure = Closure Environment Expression

instance Show Closure where
  show (Closure env body) = "with " <> show env <> "; " <> show body

-- | Union two environments. Left-biased.
unionEnv :: Environment -> Environment -> Environment
unionEnv (Environment e1) (Environment e2) = Environment (e1 `union` e2)

lookupEnv :: Text -> Environment -> Maybe Value
lookupEnv name (Environment env) = H.lookup name env

insertEnv :: Text -> Value -> Environment -> Environment
insertEnv name value (Environment env) = Environment $ H.insert name value env

singleEnv :: Text -> Value -> Environment
singleEnv name val = Environment $ H.singleton name val

-------------------------------------------------------------------------------
--------------------------------- Errors --------------------------------------
-------------------------------------------------------------------------------

data EvalError
  = NameError Text
  -- ^ If we attempt to evaluate an undefined variable.
  | TypeError (Set RuntimeType) RuntimeType
  -- ^ Type-related errors.
  | CustomError Text
  -- ^ Some custom error message.
  deriving (Show, Eq, Typeable)

instance Exception EvalError

type Result = Either EvalError

throwEvalError :: EvalError -> IO a
throwEvalError = throwIO

throwPure :: EvalError -> Result a
throwPure = Left

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
throwCustom = Left . CustomError

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
module Nix.Eval.Values where

import Nix.Common
import Nix.Eval.Expressions
import Nix.Eval.Constants
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H

-- | An environment is just a name -> value mapping.
newtype Environment = Environment {
  eEnv :: HashMap Text Value
  } deriving (Show)

-- | Union two environments. Left-biased.
unionEnv :: Environment -> Environment -> Environment
unionEnv (Environment e1) (Environment e2) = Environment (e1 `union` e2)

-- singleEnv :: Text -> Value -> Environment
-- singleEnv name val = Environment $ H.singleton name val

-- | A closure is an unevaluated expression, with just an environment.
data Closure = Closure Environment Expression

instance Show Closure where
  show (Closure env body) = "with " <> show env <> "; " <> show body

data Value
  = VConstant Constant
  | VAttrSet (HashMap Text Value)
  | VList [Value]
  | VFunction Text Closure
  | VBuiltin Text (Value -> Either Text Value)

getEnvF :: Environment -> HashMap Text Value
getEnvF (Environment env) = env

vConstant :: Constant -> Value
vConstant = VConstant

lookupEnv :: Text -> Environment -> Maybe Value
lookupEnv name (Environment env) = H.lookup name env

insertEnv :: Text -> Value -> Environment -> Environment
insertEnv name value (Environment env) = Environment $ H.insert name value env


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

--------------- BUILTINS ---------------------

type BuiltinFunc = Value -> Either Text Value

addOne :: BuiltinFunc
addOne (VConstant (Int n)) = pure $ VConstant $ Int (n + 1)
addOne v = Left "Expected an integer"

reverseString :: BuiltinFunc
reverseString (VConstant (String s)) = pure $ VConstant $ String $ reverse s
reverseString v = Left "Expected a string"

allBuiltins :: Environment
allBuiltins = Environment $ H.fromList
  [ ("addOne", VBuiltin "addOne" addOne)
  , ("reverseString", VBuiltin "reverseString" reverseString)]

{-# LANGUAGE NoImplicitPrelude #-}
module Nix.Eval.Values where

import Nix.Common
import Nix.Eval.Expressions
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H

-- | An environment is just a name -> value mapping.
newtype Environment expr val = Environment (HashMap Text (Value expr val))

-- | A closure is an unevaluated expression, with just an environment.
data Closure expr val = Closure (Environment expr val) (Expression expr)

-- | A thunk might be unevaluated (a closure) or an evaluated value. Uses
-- STrefs to provide mutability.
newtype Thunk expr val
  = Thunk (IORef (Either (Closure expr val) (Value expr val)))

data Value expr val
  = VConstant Constant
  | VAttrSet (HashMap Text val)
  | VList [val]
  | VFunction Text (Closure expr val)

newtype EnvironmentF e = EnvironmentF (Environment e (ValueF e))
newtype ValueF e = ValueF (Value e (ValueF e))

vConstant :: Constant -> Value e v
vConstant const = VConstant const

mkThunk :: Environment e v -> Expression e -> IO (Thunk e v)
mkThunk env expr = Thunk <$> newIORef (Left (Closure env expr))

lookupEnv :: Text -> Environment expr val -> Maybe (Value expr val)
lookupEnv key (Environment env) = H.lookup key env

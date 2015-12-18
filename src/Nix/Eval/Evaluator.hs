{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
module Nix.Eval.Evaluator where

import Nix.Common
import Nix.Eval.Expressions
import Nix.Eval.Values
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H

-- evalBinOp :: EnvironmentF expr
--           -> BinOp -> Expression expr -> Expression expr -> IO (ValueF expr)
-- evalBinOp env binOp left right =
--   let closure = Closure env (E

data EvalError
  = UnboundVariable Text
  deriving (Show, Eq, Typeable)

instance Exception EvalError

throwEvalError :: EvalError -> IO a
throwEvalError = throwIO

-- | Evaluate a thunk. Returns immediately if the thunk contains a Right;
-- otherwise evaluates the thunk and returns the result.
evalThunk :: Thunk expr val -> IO (Value expr val)
evalThunk (Thunk ref) = readIORef ref >>= \case
  Right evaluated -> return evaluated
  Left (Closure env expr) -> do
    result <- evaluate env expr
    writeIORef ref $ Right result
    return result

evaluate :: Environment expr val
         -> Expression expr -> IO (Value expr val)
evaluate env expr = case expr of
  EConstant const -> return $ vConstant const
  EVar name -> case lookupEnv name env of
    Nothing -> throwEvalError $ UnboundVariable name
    Just val -> return val
  ELet bindings expr' -> undefined
  _ -> error "Not defined yet"

showVal :: Value e v -> IO Text
showVal (VConstant const) = return $ tshow const
showVal (VMap const) = return $ tshow const

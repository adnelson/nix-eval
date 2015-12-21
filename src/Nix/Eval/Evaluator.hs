{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Nix.Eval.Evaluator where

import Nix.Common
import Nix.Eval.Errors
import Nix.Eval.Expressions
import Nix.Eval.Values
import Nix.Eval.RuntimeTypes
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H

evalFunc :: Environment -> Expression -> Result (Value -> Result Value)
evalFunc env funcExpr = evaluate env funcExpr >>= \case
  VFunction param (Closure cEnv body) -> pure $ \val -> do
    evaluate (insertEnv param val cEnv) body
  VBuiltin name builtin -> pure $ \val -> case builtin val of
    Left err -> Left $ BuiltinError name err
    Right val -> pure val
  v -> throwPure $ TypeError RT_Function (typeOfValue v)

-- | Evaluate an expression within an environment.
evaluate :: Environment  -- ^ Enclosing environment.
         -> Expression   -- ^ Expression to evaluate.
         -> Result Value -- ^ Result of evaluation.
evaluate env expr = case expr of
  EConstant const -> return $ vConstant const
  EVar name -> case lookupEnv name env of
    Nothing -> throwPure $ NameError name
    Just val -> return val
  ELambda param body ->
    pure $ VFunction param $ Closure env body
  EApply func arg -> do
    f <- evalFunc env func
    x <- evaluate env arg
    f x


runEval :: Expression -> Result Value
runEval e = evaluate allBuiltins e

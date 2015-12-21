{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Nix.Eval.Evaluator where

import Nix.Common
import Nix.Eval.Builtins (allBuiltins, interpretBinop)
import Nix.Eval.Expressions
import Nix.Eval.Values

-- | Evaluate the expression. If it evaluates to a function or builtin,
-- return a function which will will call that function on its argument.
-- Otherwise, return an error.
evalFunc :: Environment -> Expression -> Result (Value -> Result Value)
evalFunc env funcExpr = evaluate env funcExpr >>= \case
  VFunction param (Closure cEnv body) -> pure $ \val -> do
    evaluate (insertEnv param val cEnv) body
  VBuiltin _ builtin -> pure builtin
  v -> expectedFunction v

-- | Evaluate an expression within an environment.
evaluate :: Environment  -- ^ Enclosing environment.
         -> Expression   -- ^ Expression to evaluate.
         -> Result Value -- ^ Result of evaluation.
evaluate env expr = case expr of
  EConstant const -> return $ vConstant const
  EVar name -> case lookupEnv name env of
    Nothing -> throwPure $ NameError name
    Just val -> return val
  ELambda param body -> pure $ VFunction param $ Closure env body
  EBinaryOp left op right -> do
    let func = interpretBinop op
    leftVal <- evaluate env left
    rightVal <- evaluate env right
    func leftVal rightVal
  EApply func arg -> do
    funcVal <- evalFunc env func
    argVal <- evaluate env arg
    funcVal argVal


runEval :: Expression -> Result Value
runEval e = evaluate allBuiltins e

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Nix.Eval.Evaluator where

import Nix.Common hiding (trace)
import Nix.Eval.Builtins (allBuiltins, interpretBinop)
import Nix.Eval.Expressions
import Nix.Eval.Values

-- | Evaluate an expression within an environment.
evaluate :: Environment  -- ^ Enclosing environment.
         -> Expression   -- ^ Expression to evaluate.
         -> Result Value -- ^ Result of evaluation.
evaluate env expr = case expr of
  EConstant constant -> return $ vConstant constant
  EVar name -> case lookupEnv name env of
    Nothing -> throwPure $ NameError name env
    Just val -> val
  ELambda param body -> pure $ VFunction param $ Closure env body
  EBinaryOp left op right -> do
    let func = interpretBinop op
        leftVal = evaluate env left
        rightVal = evaluate env right
    func leftVal rightVal
  EApply func arg -> evaluate env func >>= \case
    VFunction param (Closure cEnv body) -> do
      let argVal = evaluate env arg -- Lazily evaluated!
      evaluate (insertEnv param argVal cEnv) body
    VBuiltin _ unaryFunc -> unaryFunc $ evaluate env arg
    -- Binary builtins evaluate into unary builtins.
    VBuiltin2 name binaryFunc -> do
      let res1 = evaluate env arg
          unaryFunc = binaryFunc res1
      pure $ VBuiltin (name <> " (applied)") unaryFunc
    v -> expectedFunction v
  ENonRecursiveAttrs attrs -> do
    let attrs' = VAttrSet $ Environment $ map (evaluate env) attrs
    pure attrs'
  EAttrReference attrs key -> evaluate env attrs >>= \case
    VAttrSet set -> case lookupEnv key set of
      Nothing -> throwPure $ KeyError key set
      Just res -> res
    v -> expectedAttrs v
  e -> error ("haven't done " <> show e <> " yet")

-- | Evaluate an expression with the builtins in scope.
runEval :: Expression -> Result Value
runEval e = evaluate allBuiltins e

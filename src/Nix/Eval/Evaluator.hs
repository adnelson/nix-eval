module Nix.Eval.Evaluator where

import Nix.Common hiding (trace)
import Nix.Eval.Builtins (allBuiltins, interpretBinop)
import Nix.Eval.Expressions
import Nix.Eval.Values

-- | Evaluate an expression within an environment.
evaluate :: Environment -- ^ Enclosing environment.
         -> Expression  -- ^ Expression to evaluate.
         -> LazyValue   -- ^ Result of evaluation.
evaluate env expr = case expr of
  EConstant constant -> return $ vConstant constant
  EVar name -> case lookupEnv name env of
    Nothing -> errorR $ NameError name env
    Just val -> val
  ELambda param body -> validR $ VFunction param $ Closure env body
  EBinaryOp left op right -> do
    let func = interpretBinop op
        leftVal = evaluate env left
        rightVal = evaluate env right
    applyNative func [leftVal, rightVal]
  EApply func arg -> evaluate env func >>= \case
    VNative n -> applyNative n [evaluate env arg]
    VFunction param (Closure cEnv body) -> do
      let env' = insertEnv param (evaluate env arg) cEnv
      evaluate env' body
    v -> expectedFunction v
  ENonRecursiveAttrs attrs -> do
    validR $ VAttrSet $ Environment $ map (evaluate env) attrs
  EAttrReference attrs key -> evaluate env attrs >>= \case
    VAttrSet set -> case lookupEnv key set of
      Nothing -> errorR $ KeyError key set
      Just res -> res
    v -> expectedAttrs v
  e -> error ("haven't done " <> show e <> " yet")

-- | Evaluate an expression with the builtins in scope.
runEval :: Expression -> LazyValue
runEval e = evaluate allBuiltins e

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
  ERecursiveAttrs attrs -> do
    -- Create a new environment by evaluating the values of the set.
    -- Each should be evaluated in an environment which includes the
    -- variables being evaluated; thus it is a self-referential
    -- definition. Unfortunately this means that (as currently
    -- formulated) we cannot detect infinite recursion.
    let newEnv :: Environment
        newEnv = Environment (map (evaluate newEnv) attrs) `unionEnv` env
    validR $ VAttrSet newEnv
  EAttrReference attrs key -> evaluate env attrs >>= \case
    VAttrSet set -> case lookupEnv key set of
      Nothing -> errorR $ KeyError key set
      Just res -> res
    v -> expectedAttrs v
  EWith attrs expr -> evaluate env attrs >>= \case
    -- Evaluate the attribute expression. It must be an attribute set.
    -- Bring all of those attributes into scope.
    VAttrSet set -> evaluate (set `unionEnv` env) expr
    v -> expectedAttrs v
  e -> error ("haven't done " <> show e <> " yet")

-- | Evaluate an expression with the builtins in scope.
runEval :: Expression -> LazyValue
runEval e = evaluate allBuiltins e

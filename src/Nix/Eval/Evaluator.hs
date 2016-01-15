module Nix.Eval.Evaluator where

import Nix.Common                       hiding (trace)
import Nix.Constants
import Nix.Expressions
import Nix.Eval.Builtins.Operators (interpretBinop, interpretUnop)
import Nix.Values
import Nix.Values.NativeConversion

evalApply :: LazyValue -> LazyValue -> LazyValue
evalApply func arg = func >>= \case
  VNative (NativeFunction f) -> unwrapNative =<< f arg
  VFunction param (Closure cEnv body) -> do
    let env' = insertEnvLazy param arg cEnv
    evaluate env' body
  v -> expectedFunction v

-- | Evaluate an expression within an environment.
evaluate :: LEnvironment -- ^ Enclosing environment.
         -> Expression   -- ^ Expression to evaluate.
         -> LazyValue    -- ^ Result of evaluation.
evaluate env expr = case expr of
  EConstant constant -> return $ fromConstant constant
  EVar name -> case lookupEnv name env of
    Nothing -> throwError $ NameError name (envKeySet env)
    Just val -> val
  ELambda param body -> pure $ VFunction param $ Closure env body
  EBinaryOp left op right -> do
    -- Turn the operator into a binary native function.
    let func = interpretBinop op
    -- Apply the function to the two arguments and unwrap the result.
    unwrapNative =<< (applyNative2 func (evaluate env left)
                                        (evaluate env right))
  EUnaryOp op innerExpr -> do
    -- Translate the operator into a native function.
    let func = interpretUnop op
    -- Apply the function to the inner expression.
    unwrapNative =<< applyNative func (evaluate env innerExpr)
  EApply func arg -> evaluate env func `evalApply` evaluate env arg
  EList exprs -> pure $ VList $ map (evaluate env) exprs
  ENonRecursiveAttrs attrs -> do
    pure $ VAttrSet $ Environment $ map (evaluate env) attrs
  ERecursiveAttrs attrs -> pure $ VAttrSet newEnv where
    -- Create a new environment by evaluating the values of the set.
    -- Each should be evaluated in an environment which includes the
    -- variables being evaluated; thus it is a self-referential
    -- definition. Unfortunately this means that (as currently
    -- formulated) we cannot detect infinite recursion.
    newEnv = Environment $ map (evaluate (newEnv `unionEnv` env)) attrs
  EAttrReference attrs key -> evaluate env attrs >>= \case
    VAttrSet set -> case lookupEnv key set of
      Nothing -> throwError $ KeyError key (envKeySet set)
      Just res -> res
    val -> expectedAttrs val
  EWith attrs expr -> evaluate env attrs >>= \case
    -- The expression must be an attribute set; bring into scope to
    -- evaluate the inner expression.
    VAttrSet set -> evaluate (set `unionEnv` env) expr
    val -> expectedAttrs val

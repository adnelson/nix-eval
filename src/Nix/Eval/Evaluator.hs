module Nix.Eval.Evaluator where

import Nix.Common hiding (trace)
import Nix.Eval.Constants
import Nix.Eval.Builtins (builtins)
import Nix.Eval.Operators (interpretBinop, interpretUnop)
import Nix.Eval.Expressions
import Nix.Eval.Values

-- | Maps a function over a list. Needs to be defined in this module
-- since it uses `evaluate` under the hood. This might not be necessary...
builtin_map :: Native
builtin_map = natify $ \func vlist -> case unVal vlist of
  VList list -> validR' $ VList $ map (evalApply func) list
  _ -> expectedList vlist

evalApply :: LazyValue -> LazyValue -> LazyValue
evalApply func arg = func >>= \v -> case unVal v of
  VNative n -> applyNative n [arg]
  VFunction param (Closure cEnv body) -> do
    let env' = insertEnv param arg cEnv
    evaluate env' body
  _ -> expectedFunction v

allBuiltins :: Environment
allBuiltins = insertEnv "map" (validR' $ VNative builtin_map) builtins

-- | Evaluate an expression within an environment.
evaluate :: Environment -- ^ Enclosing environment.
         -> Expression  -- ^ Expression to evaluate.
         -> LazyValue   -- ^ Result of evaluation.
evaluate env expr = case expr of
  EConstant constant -> return $ fromConstant constant
  EVar name -> case lookupEnv name env of
    Nothing -> errorR $ NameError name env
    Just val -> val
  ELambda param body -> validR' $ VFunction param $ Closure env body
  EBinaryOp left op right -> do
    let func = interpretBinop op
        leftVal = evaluate env left
        rightVal = evaluate env right
    applyNative func [leftVal, rightVal]
  EUnaryOp op expr' -> do
    let func = interpretUnop op
    applyNative func [evaluate env expr']
  EApply func arg -> evaluate env func `evalApply` evaluate env arg
  EList exprs -> validR' $ VList $ map (evaluate env) exprs
  ENonRecursiveAttrs attrs -> do
    validR' $ VAttrSet $ Environment $ map (evaluate env) attrs
  ERecursiveAttrs attrs -> validR $ Value $ VAttrSet newEnv where
    -- Create a new environment by evaluating the values of the set.
    -- Each should be evaluated in an environment which includes the
    -- variables being evaluated; thus it is a self-referential
    -- definition. Unfortunately this means that (as currently
    -- formulated) we cannot detect infinite recursion.
    newEnv = Environment $ map (evaluate (newEnv `unionEnv` env)) attrs
  EAttrReference attrs key -> do
    val <- evaluate env attrs
    case unVal val of
      VAttrSet set -> case lookupEnv key set of
        Nothing -> errorR $ KeyError key set
        Just res -> res
      _ -> expectedAttrs val
  EWith attrs expr -> do
    -- Evaluate the attribute expression. It must be an attribute set.
    val <- evaluate env attrs
    case unVal val of
      -- Bring all of those attributes into scope.
      VAttrSet set -> evaluate (set `unionEnv` env) expr
      _ -> expectedAttrs val

-- | Evaluate an expression with the builtins in scope.
runEval :: Expression -> LazyValue
runEval e = evaluate allBuiltins e

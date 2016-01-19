{-# LANGUAGE ScopedTypeVariables #-}
module Nix.Eval.Evaluator where

import Nix.Common                       hiding (trace)
import Nix.Constants
import Nix.Expressions
import Nix.Eval.Builtins.Operators (interpretBinop, interpretUnop)
import Nix.Eval.Errors
import Nix.Types (Formals(..), FormalParamSet(..))
import Nix.Values
import Nix.Values.NativeConversion
import qualified Data.Map as M
import qualified Data.Set as S

evalApply :: Monad m => LazyValue m -> LazyValue m -> LazyValue m
evalApply func arg = func >>= \case
  VNative (NativeFunction f) -> unwrapNative =<< f arg
  VFunction params (Closure cEnv body) -> case params of
    FormalName param -> do
      let env' = insertEnvL param arg cEnv
      evaluate env' body
    FormalSet params mname -> arg >>= \case
      -- We need the argument to be an attribute set to unpack arguments.
      VAttrSet argSet -> do
        let
          -- The step function will construct a new env, and also find
          -- any keys that are missing and don't have defaults.
          -- Note that we have another recursive definition here,
          -- since `step` refers to `callingEnv` and vice versa. This
          -- means that we'll loop infinitely on circular variables.
          step (newEnv, missingArgs) (key, mdef) =
              case lookupEnv key argSet of
                -- If the argument is provided, insert it.
                Just val -> (insertEnvL key val newEnv, missingArgs)
                -- If the argument is missing, see if there's a default.
                Nothing -> case mdef of
                  -- Evaluate the default expression and insert it.
                  Just def -> (insertEnvL key (evaluate callingEnv def) newEnv,
                               missingArgs)
                  -- Otherwise record it as an error.
                  Nothing -> (newEnv, key : missingArgs)
          -- Fold through the parameters with the step function.
          (callingEnv', missing) = foldl' step (cEnv, []) $ paramList params
          -- If there's a variable attached to the param set, add it
          -- to the calling environment.
          callingEnv = case mname of
            Nothing -> callingEnv'
            Just name -> insertEnvL name arg callingEnv'
        case missing of
          _:_ -> throwError $ MissingArguments missing
          _ -> case params of
            VariadicParamSet _ -> evaluate callingEnv body
            -- We need to make sure there aren't any extra arguments.
            FixedParamSet ps -> do
              let keyList :: [Text] = envKeyList argSet
                  -- For each key, we'll check if it's in the params,
                  -- and otherwise it's an `ExtraArguments` error.
                  getExtra extraKeys key = case M.lookup key ps of
                    Nothing -> (key:extraKeys)
                    Just _ -> extraKeys
              case foldl' getExtra [] keyList of
                [] -> evaluate callingEnv body
                extras -> throwError $ ExtraArguments extras
      v -> expectedAttrs v
  v -> expectedFunction v

-- | Evaluate an expression within an environment.
evaluate :: Monad m =>
            LEnvironment m -> -- ^ Enclosing environment.
            Expression     -> -- ^ Expression to evaluate.
            LazyValue m       -- ^ Result of evaluation.
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

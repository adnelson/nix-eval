{-# LANGUAGE ScopedTypeVariables #-}
module Nix.Evaluator.Evaluator where

import Control.Monad.State.Strict  --(MonadState(..), modify, execStateT)
import Nix.Common
import Nix.Constants
import Nix.Expressions
import Nix.Evaluator.Builtins.Operators (interpretBinop, interpretUnop)
import Nix.Evaluator.Errors
import Nix.Expr (Params(..), ParamSet(..), NExpr, Antiquoted(..),
                 NUnaryOp(..), NBinaryOp(..), NKeyName(..), NExprF(..),
                 NAtom(..), NString(..), Binding(..), mkSym)
import Nix.Values
import Nix.Values.NativeConversion
import qualified Data.Map as M
import qualified Data.HashMap.Strict as H
import qualified Data.Set as S

-- | Given a lazy value meant to contain a function, and a second lazy
-- value to apply that function to, perform the function application.
evalApply :: Monad m => LazyValue m -> LazyValue m -> LazyValue m
evalApply func arg = func >>= \case
  VNative (NativeFunction f) -> unwrapNative =<< f arg
  VFunction' params (Closure' cEnv body) -> case params of
    Param param -> do
      let env' = insertEnvL param arg cEnv
      evalHnix env' body
    ParamSet params mname -> arg >>= \case
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
                  Just def -> do
                    (insertEnvL key (evalHnix callingEnv def) newEnv,
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
            VariadicParamSet _ -> evalHnix callingEnv body
            -- We need to make sure there aren't any extra arguments.
            FixedParamSet ps -> do
              let keyList :: [Text] = envKeyList argSet
                  -- For each key, we'll check if it's in the params,
                  -- and otherwise it's an `ExtraArguments` error.
                  getExtra extraKeys key = case M.lookup key ps of
                    Nothing -> (key:extraKeys)
                    Just _ -> extraKeys
              case foldl' getExtra [] keyList of
                [] -> evalHnix callingEnv body
                extras -> throwError $ ExtraArguments extras
      v -> expectedAttrs v

  VFunction params (Closure cEnv body) -> case params of
    Param param -> do
      let env' = insertEnvL param arg cEnv
      evaluate env' body
    ParamSet params mname -> arg >>= \case
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

-- | Evaluate a nix string into actual text.
evalString :: Monad ctx => LEnvironment ctx -> NString NExpr -> Eval ctx Text
evalString env = \case
  DoubleQuoted strs -> concat <$> mapEval strs
  Indented strs -> intercalate "\n" <$> mapEval strs
  where mapEval = mapM (evalAntiquoted pure env)

-- | Evaluate an 'Antiquoted', resulting in 'Text'.
evalAntiquoted :: Monad ctx =>
                  (txt -> Eval ctx Text) ->
                  LEnvironment ctx ->
                  Antiquoted txt NExpr ->
                  Eval ctx Text
evalAntiquoted convertor env = \case
  Plain string -> convertor string
  Antiquoted expr -> evalHnix env expr >>= \case
    VConstant (String str) -> pure str
    v -> expectedString v

-- | Evaluate an 'NKeyName', which must result in 'Text'.
evalKeyName :: Monad ctx => LEnvironment ctx ->
               NKeyName NExpr -> Eval ctx Text
evalKeyName env = \case
  StaticKey text -> pure text
  DynamicKey antiquoted -> evalAntiquoted convertor env antiquoted
  where convertor = evalString env

--attrPathsToSet :: Monad ctx =>
--                  LEnvironment ctx ->
--                  [(Text, [Text], NExpr)] ->
--                  Eval ctx (LAttrSet ctx)
--attrPathsToSet env = loop emptyE where
--  loop set = \case
--    [] -> pure set
--    ((topKey, subKeys, expr):rest) -> do

-- | Data type to represent the state of an attribute set as it's built.
-- Values of this type are either actual lazy values ('Defined'), or they are
-- records containing other 'InProgress' objects ('InProgress').
-- This allows us to respond to this kind of syntax:
-- @{a.x = 1; b = 2; a.y = 3;}@. We can represent this intermediately as
-- @InProgress {a: InProgress {x: Defined 1, y: Defined 3}, b: Defined 2}@.
-- The idea is that when building the set, we know if something is an error
-- like a duplicate key if the key path results in a 'Defined'.
data InProgress ctx
  = Defined (LazyValue ctx)
  | InProgress (Record (InProgress ctx))

-- | Once it's finished, we can convert an 'InProgress' back into an
-- a lazy value.
convertIP :: Monad ctx => InProgress ctx -> LazyValue ctx
convertIP (Defined v) = v
convertIP (InProgress record) = do
  let items = H.toList record
      step aset (key, asip) = insertEnvL key (convertIP asip) aset
  pure $ VAttrSet $ foldl' step emptyE items


mkDot :: NExpr -> Text -> NExpr
mkDot e key = Fix $ NSelect e [StaticKey key] Nothing

insertKeyPath :: Monad ctx =>
                 [Text] ->
                 LazyValue ctx ->
                 InProgress ctx ->
                 Eval ctx (InProgress ctx)
insertKeyPath kpath lval inProg = loop kpath inProg where
  loop [] _ = pure $ Defined lval
  loop _ (Defined _) = throwError $ DuplicateKeyPath kpath
  loop (key:keys) (InProgress record) = do
    next <- loop keys $ H.lookupDefault (InProgress mempty) key record
    pure $ InProgress $ H.insert key next record

-- | Convert a list of bindings to an attribute set.
bindingsToSet :: Monad ctx =>
                 LEnvironment ctx ->
                 [Binding NExpr] ->
                 LazyValue ctx
bindingsToSet env bindings = do
  let start = InProgress mempty
  finish <- flip execStateT start $ forM_ bindings $ \case
    NamedVar keys expr -> do
      let lval = evalHnix env expr
      -- Convert the key expressions to a list of text.
      keyPath <- lift $ mapM (evalKeyName env) keys
      -- Insert the key into the in-progress set.
      get >>= lift . insertKeyPath keyPath lval >>= put
    Inherit maybeExpr keyNames -> forM_ keyNames $ \keyName -> do
      -- Evaluate the keyName to a string.
      varName <- lift $ evalKeyName env keyName
      -- Create the lazy value.
      let lval = evalHnix env $ case maybeExpr of
            Nothing -> mkSym varName
            Just expr -> mkDot expr varName
      -- Insert the keyname into the state.
      get >>= lift . insertKeyPath [varName] lval >>= put
  -- Convert the finished in-progress object into a LazyValue.
  convertIP finish

evalHnix :: Monad m =>
            LEnvironment m ->
            NExpr ->
            LazyValue m
evalHnix env (Fix expr) = do
  let recur = evalHnix env
  case expr of
    NConstant (NInt i) -> convert i
    NConstant (NBool b) -> convert b
    NConstant NNull -> pure nullV
    NConstant (NUri uri) -> convert uri
    NSym name -> case lookupEnv name env of
      Nothing -> throwError $ NameError name (envKeySet env)
      Just val -> val
    NList exprs -> pure $ VList $ fromList $ map recur exprs
    NApp func arg -> recur func `evalApply` recur arg
    NAbs param body -> pure $ VFunction' param $ Closure' env body
    NUnary op innerExpr -> do
      -- Translate the operator into a native function.
      let func = interpretUnop op
      -- Apply the function to the inner expression.
      unwrapNative =<< applyNative func (recur innerExpr)
    NBinary op left right -> do
      -- Turn the operator into a binary native function.
      let func = interpretBinop op
      -- Apply the function to the two arguments and unwrap the result.
      unwrapNative =<< applyNative2 func (recur left) (recur right)
    _ -> error $ "We don't handle " <> show expr <> " yet"

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

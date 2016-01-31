{-# LANGUAGE ScopedTypeVariables #-}
module Nix.Evaluator.Evaluator where

import Control.Monad.State.Strict  --(MonadState(..), modify, execStateT)
import Nix.Common
import Nix.Evaluator.Builtins.Operators (interpretBinop, interpretUnop)
import Nix.Evaluator.Errors
import Nix.Atoms
import Nix.Expr (Params(..), ParamSet(..), NExpr, Antiquoted(..),
                 NUnaryOp(..), NBinaryOp(..), NKeyName(..), NExprF(..),
                 NString(..), Binding(..), mkSym, (!.))
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
                  Just def -> do
                    (insertEnvL key (evaluate callingEnv def) newEnv,
                     missingArgs)
                  -- Otherwise record it as an error.
                  Nothing -> (newEnv, key : missingArgs)
          -- Fold through the parameters with the step function.
          (callingEnv', missing) = foldl' step (cEnv, []) $ case params of
            FixedParamSet params -> M.toList params
            VariadicParamSet params -> M.toList params
          -- If there's a variable attached to the param set, add it
          -- to the calling environment.
          callingEnv = case mname of
            Nothing -> callingEnv'
            Just name -> insertEnvL name arg callingEnv'
        case missing of
          -- If the missing arguments list is non-empty, it's an error.
          _:_ -> throwError $ MissingArguments missing
          _ -> case params of
            -- Variadic sets don't care about extra arguments, so we can skip
            -- checking for those.
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
  Antiquoted expr -> evaluate env expr >>= \case
    VString str -> pure str
    v -> expectedString v

-- | Evaluate an 'NKeyName', which must result in 'Text'.
evalKeyName :: Monad ctx => LEnvironment ctx ->
               NKeyName NExpr -> Eval ctx Text
evalKeyName env = \case
  StaticKey text -> pure text
  DynamicKey antiquoted -> evalAntiquoted convertor env antiquoted
  where convertor = evalString env

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

data InProgress'
  = Defined' NExpr
  | InProgress' (Record InProgress')

convertIP' :: Monad ctx =>
              LEnvironment ctx ->
              InProgress' ->
              LazyValue ctx
convertIP' env = \case
  Defined' expr -> evaluate env expr
  InProgress' record -> do
    let step aset (key, inProg) = insertEnvL key (convertIP' env inProg) aset
    pure $ VAttrSet $ foldl' step emptyE $ H.toList record


-- | Once it's finished, we can convert an 'InProgress' back into an
-- a lazy value.
convertIP :: Monad ctx => InProgress ctx -> LazyValue ctx
convertIP (Defined v) = v
convertIP (InProgress record) = do
  let items = H.toList record
      step aset (key, asip) = insertEnvL key (convertIP asip) aset
  pure $ VAttrSet $ foldl' step emptyE items

-- | Given a key path (a list of strings), stick that keypath into the in-
-- progress object we're building.
insertKeyPath' :: Monad ctx =>
                 [Text] ->
                 NExpr ->
                 InProgress' ->
                 Eval ctx InProgress'
insertKeyPath' kpath expr inProg = loop kpath inProg where
  loop [] _ = pure $ Defined' expr
  loop _ (Defined' _) = throwError $ DuplicateKeyPath kpath
  loop (key:keys) (InProgress' record) = do
    next <- loop keys $ H.lookupDefault (InProgress' mempty) key record
    pure $ InProgress' $ H.insert key next record

-- | Given a key path (a list of strings), stick that keypath into the in-
-- progress object we're building.
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

-- | Convert a list of bindings to a lazy value.
bindingsToLazyValue' :: Monad ctx =>
                       LEnvironment ctx ->
                       [Binding NExpr] ->
                       LazyValue ctx
bindingsToLazyValue' env bindings = convertIP' env =<< do
  flip execStateT (InProgress' mempty) $ forM_ bindings $ \case
    NamedVar keys expr -> do
      -- Convert the key expressions to a list of text.
      keyPath <- lift $ mapM (evalKeyName env) keys
      -- Insert the key into the in-progress set.
      get >>= lift . insertKeyPath' keyPath expr >>= put
    Inherit maybeExpr keyNames -> forM_ keyNames $ \keyName -> do
      -- Evaluate the keyName to a string.
      varName <- lift $ evalKeyName env keyName
      -- Create the actual expression.
      let expr = case maybeExpr of
            Nothing -> mkSym varName
            Just e -> e !. varName
      -- Insert the keyname into the state.
      get >>= lift . insertKeyPath' [varName] expr >>= put

-- | Convert a list of bindings to a lazy value.
bindingsToLazyValue :: Monad ctx =>
                       LEnvironment ctx ->
                       [Binding NExpr] ->
                       LazyValue ctx
bindingsToLazyValue env bindings = convertIP =<< do
  flip execStateT (InProgress mempty) $ forM_ bindings $ \case
    NamedVar keys expr -> do
      -- Convert the key expressions to a list of text.
      keyPath <- lift $ mapM (evalKeyName env) keys
      -- Insert the key into the in-progress set.
      let lval = evaluate env expr
      get >>= lift . insertKeyPath keyPath lval >>= put
    Inherit maybeExpr keyNames -> forM_ keyNames $ \keyName -> do
      -- Evaluate the keyName to a string.
      varName <- lift $ evalKeyName env keyName
      -- Create the lazy value.
      let lval = evaluate env $ case maybeExpr of
            Nothing -> mkSym varName
            Just expr -> expr !. varName
      -- Insert the keyname into the state.
      get >>= lift . insertKeyPath [varName] lval >>= put

evaluate :: Monad m => LEnvironment m -> NExpr -> LazyValue m
evaluate env (Fix expr) = do
  let recur = evaluate env -- useful shorthand
  case expr of
    NConstant atom -> pure $ VConstant atom
    NSym name -> case lookupEnv name env of
      Nothing -> throwError $ NameError name (envKeySet env)
      Just val -> val
    NList exprs -> pure $ VList $ fromList $ map recur exprs
    NApp func arg -> recur func `evalApply` recur arg
    NAbs param body -> pure $ VFunction param $ Closure env body
    NLet bindings expr' -> bindingsToLazyValue env bindings >>= \case
      VAttrSet vals -> evaluate (env `unionEnv` vals) expr'
      _ -> throwError $ FatalError BindingEvaluationFailed
    NSet bindings -> bindingsToLazyValue' env bindings
    NRecSet bindings -> bindingsToLazyValue env bindings
    NStr string -> VString <$> evalString env string
    NSelect expr' attrPath maybeDefault -> go attrPath $ recur expr' where
      go [] lval = lval
      go (keyName:keyNames) lval = lval >>= \case
        VAttrSet attrs -> do
          key <- evalKeyName env keyName
          case lookupEnv key attrs of
            Just lval' -> go keyNames lval'
            Nothing -> case maybeDefault of
              Just def -> recur def
              Nothing -> throwError $ KeyError key $ envKeySet attrs
        v -> expectedAttrs v
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

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
module Nix.Evaluator.Evaluator where

import Control.Monad.State.Strict  --(MonadState(..), modify, execStateT)
import Nix.Common
import Nix.Evaluator.Builtins.Operators (interpretBinop, interpretUnop)
import Nix.Evaluator.Errors
import Nix.Atoms
import Nix.Pretty (prettyNix)
import Nix.Expr -- (Params(..), ParamSet(..), NExpr, Antiquoted(..),
                 -- NUnaryOp(..), NBinaryOp(..), NExprF(..),
                 -- NString(..), Binding(..), mkSym, (!.))
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
  DoubleQuoted strs -> concat <$> mapM (evalAntiquoted env) strs
  Indented strs -> intercalate "\n" <$> mapM (evalAntiquoted env) strs

-- | Evaluate an 'Antiquoted', resulting in 'Text'.
evalAntiquoted :: Monad ctx =>
                  LEnvironment ctx ->
                  Antiquoted Text NExpr ->
                  Eval ctx Text
evalAntiquoted env = \case
  Plain string -> pure string
  Antiquoted expr -> evaluate env expr >>= \case
    VString str -> pure str
    v -> expectedString v

-- | Evaluate a KeyName, which must result in 'Text' or it's an error.
evalKeyName :: Monad ctx =>
               LEnvironment ctx ->
               NKeyName NExpr ->
               Eval ctx Text
evalKeyName env (DynamicKey antiq) = case antiq of
  Plain stringExpr -> evalString env stringExpr
  Antiquoted expr -> evaluate env expr >>= \case
    VString str -> pure str
    v -> expectedString v
evalKeyName _ (StaticKey txt) = pure txt

-- | Data type to represent the state of an attribute set as it's built.
-- Values of this type are either actual expressions ('Defined'), or they are
-- records containing other 'InProgress' objects ('InProgress').
-- This allows us to respond to this kind of syntax:
-- @{a.x = 1; b = 2; a.y = 3;}@. We can represent this intermediately as
-- @InProgress {a: InProgress {x: Defined 1, y: Defined 3}, b: Defined 2}@.
-- The idea is that when building the set, we know if something is an error
-- like a duplicate key if the key path results in a 'Defined'.
data InProgress
  = Defined NExpr
  | InProgress (Record InProgress)

-- | Once it's finished, we can convert an 'InProgress' into a lazy value.
convertIP :: Monad ctx => LEnvironment ctx -> InProgress -> LazyValue ctx
convertIP env = \case
  Defined expr -> evaluate env expr
  InProgress record -> pure $ VAttrSet $ recordIPToEnv env record

recordIPToEnv :: Monad ctx => LEnvironment ctx -> Record InProgress ->
                 LEnvironment ctx
recordIPToEnv env record = do
  let step aset (key, inProg) = insertEnvL key (convertIP env inProg) aset
  foldl' step emptyE $ H.toList record

-- | Given a key path (a list of strings), stick that keypath into the in-
-- progress object we're building.
insertKeyPath :: Monad ctx => [Text] -> NExpr -> InProgress ->
                 Eval ctx InProgress
insertKeyPath kpath expr inProg = loop kpath inProg where
  -- Insering a key. Recur on the remaining keys to build up an in-progress,
  -- and then add that under the given key.
  loop (key:keys) (InProgress record) = do
    next <- loop keys $ H.lookupDefault (InProgress mempty) key record
    pure $ InProgress $ H.insert key next record
  -- If we encounter a defined expression here, it means the key path was
  -- duplicated, which is an error.
  loop (_:_) (Defined _) = throwError $ DuplicateKeyPath kpath
  -- If we're out of keys to traverse, we stick in the expression.
  loop [] _ = pure $ Defined expr

-- | Given a key path (a list of strings), stick that keypath into the in-
-- progress object we're building.
insertKeyPathRecord :: Monad ctx =>
                       [Text] ->
                       NExpr ->
                       Record InProgress ->
                       Eval ctx (Record InProgress)
insertKeyPathRecord kpath expr record = do
  insertKeyPath kpath expr (InProgress record) >>= \case
    Defined _ -> throwError $ FatalError EmptyKeyPath
    InProgress record' -> pure record'

buildInProgressRecord :: Monad ctx =>
                         LEnvironment ctx ->
                         [Binding NExpr] ->
                         Eval ctx (Record InProgress)
buildInProgressRecord env bindings = flip execStateT mempty $
  forM_ bindings $ \case
    NamedVar (keys :: [NKeyName NExpr]) expr -> do
      -- Convert the key expressions to a list of text.
      keyPath <- lift $ mapM (evalKeyName env) keys
      -- Insert the key into the in-progress set.
      record <- get
      record' <- lift $ insertKeyPathRecord keyPath expr record
      put record'
    Inherit maybeExpr keyNames -> forM_ keyNames $ \keyName -> do
      -- Evaluate the keyName to a string.
      varName <- lift $ evalKeyName env keyName
      -- Create the actual expression.
      let expr = case maybeExpr of
            Nothing -> mkSym varName
            Just e -> e !. varName
      -- Insert the keyname into the state.
      get >>= lift . insertKeyPathRecord [varName] expr >>= put

-- | Convert a list of bindings to an environment. The bindings can optionally
-- be recursive, meaning that one binding in the list can refer to another.
bindingsToEnv :: Monad ctx =>
                 Bool ->
                 LEnvironment ctx ->
                 [Binding NExpr] ->
                 Eval ctx (LEnvironment ctx)
bindingsToEnv recursively env bindings = case recursively of
  False -> recordIPToEnv env <$> buildInProgressRecord env bindings
  True -> do
    -- mergedEnv <- pure $ env `unionEnv` env'
    -- env' <- recordIPToEnv env <$> buildInProgressRecord mergedEnv bindings
    pure emptyE -- env'

evaluate :: Monad m => LEnvironment m -> NExpr -> LazyValue m
evaluate env (Fix expr) = do
  let recur = evaluate env -- useful shorthand
  case expr of
    NSym name -> case lookupEnv name env of
      Nothing -> throwError $ NameError name (envKeySet env)
      Just val -> val
    NConstant atom -> pure $ VConstant atom
    NStr string -> VString <$> evalString env string
    NList exprs -> pure $ VList $ fromList $ map recur exprs
    NApp func arg -> recur func `evalApply` recur arg
    NAbs param body -> pure $ VFunction param $ Closure env body
    NLet bindings expr' -> do
      env' <- bindingsToEnv True env bindings
      evaluate (env `unionEnv` env') expr'
    NSet bindings -> VAttrSet <$> bindingsToEnv False env bindings
    NRecSet bindings -> VAttrSet <$> bindingsToEnv True env bindings
    NSelect expr' attrPath maybeDefault -> do
      go attrPath $ recur expr' where
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

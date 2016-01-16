module Nix.Values.Strict where

import Nix.Common
import Nix.Expressions
import Nix.Constants
import Nix.Values.Generic
import Nix.Values.Lazy

-- | Strict values are fully evaluated (at least conceptually);
-- internally they only contain other strict values. This means, among
-- other things, that they can be tested for equality.
type StrictValue = Value Identity

-- | Because the 'Value' type requires a context, we use the 'Identity'
-- type as a "context-free context".
type StrictValue' = Identity (Value Identity)

-- | An environment containing strict values.
type SEnvironment = Environment Identity

-- | Strict values can be converted into values in WHNF, since
-- there is at that point no chance of failures (unless we encounter a
-- native function on strict values, which will never happen unless our
-- code structure changes dramatically at some point...)
strictToLazy :: Monad m => StrictValue -> WHNFValue m
strictToLazy = \case
  VConstant c -> VConstant c
  VAttrSet attrs -> VAttrSet $ transE attrs
  VList vals -> VList $ map trans vals
  VFunction p (Closure env e) -> VFunction p (Closure (transE env) e)
  VNative (NativeValue v) -> VNative $ NativeValue $ trans v
  VNative (NativeFunction _) -> error "Can't convert native functions"
  where trans (Identity s) = return $ strictToLazy s
        transE (Environment env) = Environment $ map trans env

-- | Lazy values can be converted into 'StrictValue's. However, there
-- is a chance that this conversion will fail, because the input might
-- produce an error upon evaluation. So we can only make things total
-- by returning in the 'Eval' monad.
whnfToStrict :: Monad m => WHNFValue m -> Eval m StrictValue
whnfToStrict val = case val of
  VConstant c -> pure $ VConstant c
  VAttrSet attrs -> VAttrSet <$> lazyEnvToStrictEnv attrs
  VList lvals -> VList <$> do
    strictVals <- mapM lazyToStrict lvals
    return $ map return strictVals
  VFunction param (Closure env body) -> do
    sEnv <- lazyEnvToStrictEnv env
    return $ VFunction param (Closure sEnv body)
  VNative (NativeValue nval) -> whnfToStrict =<< nval
  VNative (NativeFunction _) -> do
    throwError $ CustomError "Can't make a native function strict"

-- | Convert a lazy Environment to one containing only strict values.
lazyEnvToStrictEnv :: Monad m => LEnvironment m -> Eval m SEnvironment
lazyEnvToStrictEnv env = foldM step emptyE (envToList env) where
  step res (name, lval) = do
    sval <- lazyToStrict lval
    return (insertEnv name sval res)

lazyToStrict :: Monad m => LazyValue m -> Eval m StrictValue
lazyToStrict lval = lval >>= whnfToStrict

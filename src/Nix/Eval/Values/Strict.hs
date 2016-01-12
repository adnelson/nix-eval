module Nix.Eval.Values.Strict where

import Nix.Common
import Nix.Eval.Expressions
import Nix.Eval.Constants
import Nix.Eval.Values.Generic
import Nix.Eval.Values.Lazy

-- | Strict values are fully evaluated (at least conceptually);
-- internally they only contain other strict values. This means, among
-- other things, that they can be tested for equality.
type StrictValue = Value Identity

-- | Because the 'Value' type requires a context, we use the 'Identity'
-- type as a "context-free context".
type StrictValue' = Identity (Value Identity)

-- Some type synonyms for readability. The 'S' is for strict.
type SNative = Native Identity
type SEnvironment = Environment Identity
type SAttrSet = AttrSet Identity
type SClosure = Closure Identity

-- | Strict values can be converted into values in WHNF, since
-- there is at that point no chance of failures.
strictToLazy :: StrictValue -> WHNFValue
strictToLazy = \case
  VConstant c -> VConstant c
  VAttrSet attrs -> VAttrSet $ transE attrs
  VList vals -> VList $ map trans vals
  VFunction p (Closure env e) -> VFunction p (Closure (transE env) e)
  VNative (NativeValue v) -> VNative $ NativeValue $ trans v
  VNative (NativeFunction f) -> error "Can't convert native functions"
  where trans (Identity s) = return $ strictToLazy s
        transE (Environment env) = Environment $ map trans env

-- | Lazy values can be converted into 'StrictValue's. However, there
-- is a chance that this conversion will fail, because the input might
-- produce an error upon evaluation. So we can only make things total
-- by returning in the 'Eval' monad.
valueToStrictValue :: WHNFValue -> Eval StrictValue
valueToStrictValue val = case val of
  VConstant c -> pure $ VConstant c
  VAttrSet attrs -> VAttrSet <$> lazyEnvToStrictEnv attrs
  VList lvals -> VList <$> do
    strictVals <- mapM lazyToStrict lvals
    return $ map return strictVals
  VFunction param (Closure env body) -> do
    sEnv <- lazyEnvToStrictEnv env
    return $ VFunction param (Closure sEnv body)
  VNative (NativeValue nval) -> valueToStrictValue =<< nval
  VNative (NativeFunction _) -> do
    throwError $ CustomError "Can't make a native function strict"

-- | Convert a lazy Environment to one containing only strict values.
lazyEnvToStrictEnv :: LEnvironment -> Eval SEnvironment
lazyEnvToStrictEnv env = foldM step emptyE (envToList env) where
  step res (name, lval) = do
    sval <- lazyToStrict lval
    return (insertEnv name sval res)

lazyToStrict :: LazyValue -> Eval StrictValue
lazyToStrict lval = lval >>= valueToStrictValue

-- lazyNativeToStrict :: LNative LazyValue -> Eval (SNative StrictValue')
-- lazyNativeToStrict (NativeValue lval) = NativeValue <$> lazyToStrict lval

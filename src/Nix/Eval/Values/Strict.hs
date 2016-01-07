module Nix.Eval.Values.Strict where

import Nix.Common
import Nix.Eval.Expressions
import Nix.Eval.Constants
import Nix.Eval.Values.Generic
import Nix.Eval.Values.Lazy

-- | Strict values are fully evaluated (at least conceptually);
-- internally they only contain other strict values. This means, among
-- other things, that they can be tested for equality.
newtype StrictValue = StrictValue (Value' StrictValue)
  deriving (Generic, Show, Eq)

-- | Strict values can be converted into lazy values, since
-- there is at that point no chance of failures.
strictToLazy :: StrictValue -> LazyValue
strictToLazy (StrictValue sval) = undefined

-- | 'Value's and 'LazyValue's can be converted into 'StrictValue's.
-- However, there is a chance that this conversion will fail, because
-- the input might produce an error upon evaluation. So returning a
-- 'Result' is best.
valueToStrictValue :: Value -> Result StrictValue
valueToStrictValue val = case unVal val of
  VConstant c -> pure $ StrictValue $ VConstant c
  VAttrSet attrs -> StrictValue . VAttrSet <$> lazyEnvToStrictEnv attrs
  VList lvals -> StrictValue . VList <$> mapM lazyToStrict lvals
  VFunction param (Closure env body) -> do
    sEnv <- lazyEnvToStrictEnv env
    return $ StrictValue $ VFunction param (Closure sEnv body)
  VNative native -> StrictValue . VNative <$> lazyNativeToStrict native

-- | Convert a lazy Environment to one containing only strict values.
lazyEnvToStrictEnv :: Environment -> Result (Environment' StrictValue)
lazyEnvToStrictEnv env = foldM step emptyE (envToList env) where
  step res (name, lval) = do
    sval <- lazyToStrict lval
    return (insertEnv name sval res)

lazyToStrict :: LazyValue -> Result StrictValue
lazyToStrict lval = lval >>= valueToStrictValue

lazyNativeToStrict :: Native -> Result (Native' StrictValue)
lazyNativeToStrict (NativeValue lval) = NativeValue <$> lazyToStrict lval
lazyNativeToStrict (NativeFunction _) = do
  -- This might not be the correct approach, but it's ok for now.
  error "Can't make strict native functions"

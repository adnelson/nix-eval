module Nix.Values.NativeConversion where

import Nix.Common
import Nix.Values.Generic
import Nix.Values.Lazy

-- | Aliases a function on 'WHNFValue's of arity 1.
type WHNFFunc1 = WHNFValue -> WHNFValue

-- | Aliases a function on 'WHNFValue's of arity 2.
type WHNFFunc2 = WHNFValue -> WHNFValue -> WHNFValue

-- | Takes a function which takes a strictly-evaluated value and turns
-- it into a 'Native' value. Note that we need to inspect the inner
-- value (hence the 'map') before the function can be applied, so this
-- has the effect of forcing WHNF evaluation.
toNative1 :: (WHNFValue -> LazyValue) -> LNative WHNFFunc1
toNative1 f = NativeFunction $ map (NativeValue . f)

-- | Similar to 'toNative1', except that it does not need to unpack
-- the first argument value (and hence potentially fail), so the
-- initial value stays lazy.
toNative1L :: (LazyValue -> LazyValue) -> LNative WHNFFunc1
toNative1L f = NativeFunction $ return . NativeValue . f

-- | For arity-2 functions on WHNF values.
toNative2 :: (WHNFValue -> WHNFValue -> LazyValue) -> LNative WHNFFunc2
toNative2 f = NativeFunction $ \lazyVal -> do
  val <- lazyVal
  return $ toNative1 $ f val

-- | For arity-2 functions whose /second/ argument can be lazily evaluated.
toNative2L :: (WHNFValue -> LazyValue -> LazyValue) -> LNative WHNFFunc2
toNative2L f = NativeFunction $ (=<<) (return . toNative1L . f)

-- | For arity-2 functions whose /first/ argument can be lazily evaluated.
toNative2L' :: (LazyValue -> WHNFValue -> LazyValue) -> LNative WHNFFunc2
toNative2L' f = NativeFunction $ return . toNative1 . f

module Nix.Values.NativeConversion where

import Nix.Common
import Nix.Constants
import Nix.Values.Generic
import Nix.Values.Lazy

-- | Aliases a function on 'WHNFValue's of arity 1.
type NativeFunc1 m = LNative m (WHNFValue m -> WHNFValue m)

-- | Aliases a function on 'WHNFValue's of arity 2.
type NativeFunc2 m = LNative m (WHNFValue m -> WHNFValue m -> WHNFValue m)

-- | Takes a function which takes a strictly-evaluated value and turns
-- it into a 'Native' value. Note that we need to inspect the inner
-- value (hence the 'map') before the function can be applied, so this
-- has the effect of forcing WHNF evaluation.
toNative1 :: Monad m => (WHNFValue m -> LazyValue m) -> NativeFunc1 m
toNative1 f = NativeFunction $ map (NativeValue . f)

-- | Similar to 'toNative1', except that it does not need to unpack
-- the first argument value (and hence potentially fail), so the
-- initial value stays lazy.
toNative1L :: Monad m => (LazyValue m -> LazyValue m) -> NativeFunc1 m
toNative1L f = NativeFunction $ return . NativeValue . f

-- | For arity-2 functions on WHNF values.
toNative2 :: Monad m =>
             (WHNFValue m -> WHNFValue m -> LazyValue m) ->
             NativeFunc2 m
toNative2 f = NativeFunction $ \lazyVal -> do
  val <- lazyVal
  return $ toNative1 $ f val

-- | For arity-2 functions whose /second/ argument can be lazily evaluated.
toNative2L :: Monad m =>
              (WHNFValue m -> LazyValue m -> LazyValue m) ->
              NativeFunc2 m
toNative2L f = NativeFunction $ (=<<) (return . toNative1L . f)

-- | For arity-2 functions whose /first/ argument can be lazily evaluated.
toNative2L' :: Monad m =>
               (LazyValue m -> WHNFValue m -> LazyValue m) ->
               NativeFunc2 m
toNative2L' f = NativeFunction $ return . toNative1 . f

-- WIP
-- intFunc :: Monad m => (Integer -> LazyValue m) -> WHNFValue m -> LazyValue m
-- intFunc func (VConstant (Int i)) = func i
-- intFunc _ v = expectedInt v

-- intFunc2 :: Monad m =>
--             (Integer -> Integer -> LazyValue m) ->
--             WHNFValue m -> WHNFValue m -> LazyValue m
-- intFunc2 func (VConstant (Int i)) = intFunc (func i)

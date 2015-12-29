{-# LANGUAGE GADTs #-}
module Nix.Eval.NativeGADT where

import Nix.Eval.Values

-- | Why not a GADT?
-- I'm just going to leave this here because it provides an
-- alternative to the type above, but I'm not sure it's desirable.
data Native' t where
  NativeValue' :: LazyValue -> Native' LazyValue
  -- ^ A lazy value can be treated as a native.
  NativeFunction' :: (LazyValue -> Native' t) -> Native' (LazyValue -> t)

myNativeFunc :: Native' (LazyValue -> LazyValue)
myNativeFunc = NativeFunction' $ \(Result res) -> case res of
  Left err -> NativeValue' $ errorR err
  Right val -> NativeValue' $ validR val

myNativeFunc2 :: Native' (LazyValue -> LazyValue -> LazyValue)
myNativeFunc2 = NativeFunction' $ \(Result res) -> case res of
  Left err -> NativeFunction' $ \_ -> NativeValue' $ errorR err
  Right _ -> myNativeFunc

class Natify' t where
  natify' :: t -> Native' t

instance Natify' LazyValue where
  natify' val = NativeValue' val

instance Natify' t => Natify' (LazyValue -> t) where
  natify' function = NativeFunction' $ \lval -> natify' (function lval)

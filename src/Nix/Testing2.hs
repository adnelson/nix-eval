{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Nix.Testing2 where

import ClassyPrelude

data Value v = Value v
newtype Result a = Result (Either String a)
  deriving (Functor, Applicative, Monad)
data WHNFValue = WHNFValue (Value LazyValue)
data LazyValue = LazyValue (Result WHNFValue)

-- | An embedding of raw values. Lets us write functions in Haskell
-- which operate on Nix values, and expose these in Nix code.
data Native eval v
  = NativeValue (eval v)
  -- ^ A terminal value (or an error)
  | NativeFunction (eval v -> eval (Native eval v))
  -- ^ A function which lets us take the "next step" given a value.
  -- The input is lazy, so it's in the eval monad. Similarly, the
  -- result of the function might be an error, and so is a 'result'.
  deriving (Generic)

-- | The relation @Natify t v@ states that given a value of type @t@,
-- I can produce a value of type @Native v@.
class Natify t v | t -> v where
  natify :: Monad eval => t -> Native eval v

-- | This states the (obvious) fact that given a native, I can produce
-- another native value of the same type.
-- instance Natify (Native eval v) v where
--  natify n = _id

-- | Given a LazyValue, I can produce another LazyValue. Note that I'm
-- being explicit here because it's not always the case that we want a
-- @t@ to produce a @Native t@.
instance Natify LazyValue WHNFValue where
  natify (LazyValue res) = NativeValue res

-- | Given a value in WHNF, I can make a lazy native value.
instance Natify WHNFValue WHNFValue where
  natify = NativeValue . return

instance Natify t WHNFValue => Natify (LazyValue -> t) WHNFValue where
  natify function = NativeFunction $ \lazy -> do
    -- There is no need to evaluate the lazy value here; we can pass
    -- it directly into our function, getting a @t@ back which, by our
    -- assumption earlier, we can translate into a 'Native WHNFValue'.
    let nativeResult = natify $ function $ LazyValue lazy
    -- Having done this, we can return the value -- note that no
    -- computation in the Result monad has taken place!
    return nativeResult

-- | This instance of Natify demonstrates how we need to evaluate the
-- lazy value to extract the value out of it.
instance Natify t WHNFValue => Natify (WHNFValue -> t) WHNFValue where
  natify function = NativeFunction $ \lazy -> do
    -- In contrast to the lazy instance, we're now forced to
    -- /evaluate/ the lazy value, meaning we could fail.
    val <- lazy
    return $ natify $ function val

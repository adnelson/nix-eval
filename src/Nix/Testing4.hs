{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Nix.Testing4 where

import ClassyPrelude

data Value :: (* -> *) -> * -> * where
  Value :: v -> Value m v
newtype Result a = Result (Either String a)
  deriving (Functor, Applicative, Monad)
data WHNFValue = WHNFValue (Value Result LazyValue)
data LazyValue = LazyValue (Result WHNFValue)

-- | An embedding of raw values. Lets us write functions in Haskell
-- which operate on Nix values, and expose these in Nix code.
data Native m v
  = NativeValue (m v)
  -- ^ A terminal value (or an error)
  | NativeFunction (m v -> m (Native m v))
  -- ^ A function which lets us take the "next step" given a value.
  -- The input is lazy, so it's monadic. Similarly, the result of
  -- the function might be an error, and so is also monadic.
  deriving (Generic)

-- | The relation @Natify t v@ states that given a value of type @t@,
-- I can produce a value of type @Native v@.
class Natify t where
  type EvalResult t :: *
  type EvalMonad t :: * -> *
  natify :: t -> Native (EvalMonad t) (EvalResult t)

-- | This states the (obvious) fact that given a native, I can produce
-- another native value of the same type.
instance Natify (Native m v) where
  type EvalResult (Native m v) = v
  type EvalMonad (Native m v) = m
  natify = id

-- | Given a LazyValue, I can produce another LazyValue. Note that I'm
-- being explicit here because it's not always the case that we want a
-- @t@ to produce a @Native t@.
instance Natify LazyValue where
  type EvalResult LazyValue = WHNFValue
  type EvalMonad LazyValue = Result
  natify (LazyValue res) = NativeValue res

-- | Given a value in WHNF, I can make a lazy native value.
instance Natify WHNFValue where
  type EvalResult WHNFValue = WHNFValue
  type EvalMonad WHNFValue = Result
  natify = NativeValue . return

-- | This instance illustrates why lazy values are lazy: we can make
-- a Native out of functions which take LazyValues and we don't need
-- to inpect the contents of (i.e. evaluate) the result of the lazy
-- value to do so.
instance (Natify t, EvalResult t ~ WHNFValue, EvalMonad t ~ Result)
         => Natify (LazyValue -> t) where
  type EvalResult (LazyValue -> t) = WHNFValue
  type EvalMonad (LazyValue -> t) = Result
  natify function = NativeFunction $ \lazy -> do
    -- There is no need to evaluate the lazy value here; we can pass
    -- it directly into our function, getting a @t@ back which, by our
    -- assumption earlier, we can translate into a 'Native WHNFValue'.
    let nativeResult = natify $ function $ LazyValue lazy
    -- Having done this, we can return the value -- note that no
    -- computation in the Result monad has taken place!
    return nativeResult

-- | This instance of Natify demonstrates how we need to evaluate the
-- lazy value to extract the value out of it. Only once we have
-- evaluated our result can we find out what the next thing to do is.
instance (Natify t, EvalResult t ~ WHNFValue, EvalMonad t ~ Result)
         => Natify (WHNFValue -> t) where
  type EvalResult (WHNFValue -> t) = WHNFValue
  type EvalMonad (WHNFValue -> t) = Result
  natify function = NativeFunction $ \lazy -> do
    -- In contrast to the lazy instance, we're now forced to
    -- /evaluate/ the lazy value, meaning we could fail.
    val <- lazy
    return $ natify $ function val

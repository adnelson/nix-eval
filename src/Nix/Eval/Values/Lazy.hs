{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Nix.Eval.Values.Lazy where

import Nix.Common
import Nix.Eval.Expressions
import Nix.Eval.Constants
import Nix.Eval.Values.Generic
import qualified Data.HashMap.Strict as H

-- | The result of evaluation: it might be an error. In Haskell, this
-- has the effect of creating lazy evaluation, as what is /inside/ the
-- result is not evaluated until inspected at some point.
newtype Eval a = Eval (Either (EvalError Eval) a)
  deriving (Functor, Applicative, Monad, Generic)

-- | Weak-head-normal-form values are strict at the top-level, but
-- internally contains lazily evaluated values.
type WHNFValue = Value Eval

-- | This is how we represent a lazily evaluated value: It's a
-- 'Result', which means it might be an error; but if it's not an
-- error it will be a value in WHNF.
type LazyValue = Eval WHNFValue

-- Some type synonyms for readability. The 'L' is for lazy.
type LNative = Native Eval
type LEnvironment = Environment Eval
type LAttrSet = AttrSet Eval
type LClosure = Closure Eval

-- | Wrap a value in a result.
validR :: WHNFValue -> LazyValue
validR = return

-- | Shorthand for creating an Environment from a list.
mkEnv :: [(Text, WHNFValue)] -> LEnvironment
mkEnv = Environment . H.fromList . map (map validR)

-- | Shorthand to create a closure from a list and an expression.
mkClosure :: [(Text, WHNFValue)] -> Expression -> LClosure
mkClosure env expr = Closure (mkEnv env) expr

-- | Create a value from a string.
strV :: Text -> WHNFValue
strV = fromConstant . String

-- | Create a value from an integer.
intV :: Integer -> WHNFValue
intV = fromConstant . Int

-- | Create a value from a bool.
boolV :: Bool -> WHNFValue
boolV = fromConstant . Bool

-- | Create a null value.
nullV :: WHNFValue
nullV = fromConstant Null

-- | Create an attribute set value.
attrsV :: [(Text, WHNFValue)] -> WHNFValue
attrsV = VAttrSet . mkEnv

-- | Create a list value.
listV :: [WHNFValue] -> WHNFValue
listV = VList . fromList . map validR

-- | Create a function value.
functionV :: Text -> LClosure -> WHNFValue
functionV param closure = VFunction param closure

-- -- | Create a native value.
-- nativeV :: Natify n Eval => n -> WHNFValue
-- nativeV = WHNFValue . VNative . natify

-- -- | We can turn any lazy value into a native.
-- instance Natify LazyValue Eval where
--   natify = NativeValue

-- -- | This is where things get interesting: a 'Natify' instance for
-- -- functions on lazy values lets us embed any function on 'LazyValue's
-- -- as a 'Native' function. So for example, we can 'natify' a function
-- -- of type @'LazyValue' -> -- 'LazyValue'@, or @'LazyValue' ->
-- -- 'LazyValue' -> 'LazyValue'@, etc.
-- instance Natify t Eval => Natify (WHNFValue -> t) Eval where
--   natify function = NativeFunction $ _huh -- \lval -> do
--     -- let res :: t
--     --     res = function _lval
--     -- natify _what

-- -- | We can 'natify' an arbitrary function on values (provided its
-- -- return type implements 'Natify'). However, it has the effect of
-- -- forcing strict evaluation, as the only way to extract the inner
-- -- value is to evaluate the 'Result' wrapper to WHNF.
-- instance (Natify a m, Natify b m) => Natify (a -> b) m where
--   natify function = do
--     NativeFunction $ \x -> do
--       -- let y = natify (function x)
--       undefined

-------------------------------------------------------------------------------
--------------------------- Deep Evaluation -----------------------------------
-------------------------------------------------------------------------------

-- | Most of the time we evaluate things lazily, but sometimes we want
-- to be able to evaluate strictly (esp. in the `deepSeq` function).
deeplyEval :: WHNFValue -> LazyValue
deeplyEval val = case val of
  VNative (NativeValue nval) -> join $ deeplyEvalLazy nval
  VList lvals -> map VList $ mapM deeplyEvalLazy lvals
  VAttrSet (Environment attrs) -> do
    VAttrSet . Environment <$> mapM deeplyEvalLazy attrs
  -- Note that we don't recur into functions (or NativeFunctions), and
  -- since constants can't fail there's no need to handle them either.
  _ -> return val

-- | Deeply evaluate a lazy value, while keeping it appearing as lazy.
deeplyEvalLazy :: LazyValue -> Eval LazyValue
deeplyEvalLazy lval = lval >>= deeplyEval >> return lval

-- instance FromConstant LazyValue where
--   fromConstant = validR . fromConstant
--   fromConstants = validR . fromConstants
--   fromConstantSet = validR . fromConstantSet

-- | Synonym for monadic bind, applying a function inside of a
-- 'Result', provided the 'Result' contains a 'WHNFValue'.
unwrapAndApply :: (WHNFValue -> LazyValue) -> LazyValue -> LazyValue
unwrapAndApply func res = res >>= func

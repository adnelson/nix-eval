{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Nix.Eval.Values.Lazy where

import Nix.Common
import Nix.Eval.Expressions
import Nix.Eval.Constants
import Nix.Eval.Values.Generic
import qualified Data.HashMap.Strict as H


-- | Weak-head-normal-form values are strict at the top-level, but
-- internally contains lazily evaluated values.
newtype WHNFValue = WHNFValue {unVal :: Value LEval LazyValue}
  deriving (Show, Eq)

-- | This is how we represent a lazily evaluated value: It's a
-- 'Result', which means it might be an error; but if it's not an
-- error it will be a value in WHNF.
type LazyValue = LEval WHNFValue

-- Some type synonyms for readability. The 'L' is for lazy.
-- | Our evaluation context; it might error or return a value in WHNF.
type LEval = Eval WHNFValue
type LNative = Native LEval WHNFValue
type LEnvironment = Environment WHNFValue
type LAttrSet = AttrSet WHNFValue
type LClosure = Closure WHNFValue

instance HasRTType WHNFValue LEval where
  typeOf (WHNFValue v) = typeOf v

-- | Wrap a value in a result.
validR :: WHNFValue -> LazyValue
validR = return

validR' :: Value LEval LazyValue -> LazyValue
validR' = validR . WHNFValue

-- | Shorthand for creating an Environment from a list.
mkEnv :: [(Text, WHNFValue)] -> Environment LazyValue
mkEnv = Environment . H.fromList . map (map validR)

-- | Shorthand to create a closure from a list and an expression.
mkClosure :: [(Text, WHNFValue)] -> Expression -> LClosure
mkClosure env expr = Closure (mkEnv env) expr

-- | Apply a native value as if it were a function, to zero or more arguments.
applyNative :: LNative -> [LazyValue] -> LazyValue
applyNative native [] = unwrapNative native
applyNative native (rval:rvals) = case native of
  NativeFunction func -> applyNative (func rval) rvals
  NativeValue v -> expectedFunction v

-- | Turn an 'LNative' into an 'LazyValue'. This might rewrap it in a
-- 'VNative' constructor, if the object is not a 'NativeValue'.
unwrapNative :: LNative -> LazyValue
unwrapNative (NativeValue rv) = rv
unwrapNative n = validR $ WHNFValue $ VNative n

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
attrsV = WHNFValue . VAttrSet . mkEnv

-- | Create a list value.
listV :: [WHNFValue] -> WHNFValue
listV = WHNFValue . VList . fromList . map validR

-- | Create a function value.
functionV :: Text -> LClosure -> WHNFValue
functionV param closure = WHNFValue $ VFunction param closure

-- -- | Create a native value.
-- nativeV :: Natify n LEval => n -> WHNFValue
-- nativeV = WHNFValue . VNative . natify

-- -- | We can turn any lazy value into a native.
-- instance Natify LazyValue LEval where
--   natify = NativeValue

-- -- | This is where things get interesting: a 'Natify' instance for
-- -- functions on lazy values lets us embed any function on 'LazyValue's
-- -- as a 'Native' function. So for example, we can 'natify' a function
-- -- of type @'LazyValue' -> -- 'LazyValue'@, or @'LazyValue' ->
-- -- 'LazyValue' -> 'LazyValue'@, etc.
-- instance Natify t LEval => Natify (WHNFValue -> t) LEval where
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
deeplyEval v@(WHNFValue val) = case val of
  VNative (NativeValue val') -> deeplyEvalLazy val'
  VList lvals -> WHNFValue . VList <$> foldM step mempty lvals where
    step = undefined
    -- -- Recur on items of the list, and check if any are errors.
    -- let (_, errs) = break (isError . deeplyEvalLazy) lvals
    -- case toList errs of
    --   [] -> return v -- no errors, can just return the thing
    --   (err:_) -> errorR err -- return the first error found
  VAttrSet attrs -> WHNFValue . VAttrSet <$> foldM step emptyE (envToList attrs) where
    step = undefined
    -- let (_, errs) = break (isError . deeplyEvalLazy) $ H.elems lvals
    -- case errs of
    --   [] -> return v -- no errors, can just return the thing
    --   (err:_) -> _errorR err -- return the first error found
  -- Note that we don't recur into functions (or NativeFunctions), and
  -- since constants can't fail there's no need to handle them either.
  _ -> return v

-- | Deeply evaluate a lazy value, while keeping it appearing as lazy.
deeplyEvalLazy :: LazyValue -> LazyValue
deeplyEvalLazy lval = lval >>= deeplyEval

instance FromConstant LazyValue where
  fromConstant = validR . fromConstant
  fromConstants = validR . fromConstants
  fromConstantSet = validR . fromConstantSet

instance FromConstant WHNFValue where
  fromConstant = WHNFValue . fromConstant
  fromConstants = WHNFValue . fromConstants
  fromConstantSet = WHNFValue . fromConstantSet

-- | Synonym for monadic bind, applying a function inside of a
-- 'Result', provided the 'Result' contains a 'WHNFValue'.
unwrapAndApply :: (WHNFValue -> LazyValue) -> LazyValue -> LazyValue
unwrapAndApply func res = res >>= func

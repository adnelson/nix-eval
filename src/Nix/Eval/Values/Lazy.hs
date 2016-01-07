{-# LANGUAGE FlexibleInstances #-}
module Nix.Eval.Values.Lazy where

import Nix.Common
import Nix.Eval.Expressions
import Nix.Eval.Constants
import Nix.Eval.Values.Generic
import qualified Data.HashMap.Strict as H


type Result = Result' LazyValue

-- | Weak-head-normal-form values are strict at the top-level, but
-- internally may contain lazily evaluated values.
newtype Value = Value {
  unVal :: Value' LazyValue
  } deriving (Show, Eq)

-- | This is how we represent a lazily evaluated value: It's a
-- 'Result', which means it might be an error; but if it's not an
-- error it will be a value in WHNF.
newtype LazyValue = LazyValue {
  unLVal :: Result Value
  } deriving (Show, Eq)

-- | The most common use of natives is to encode lazy values.
type Native = Native' LazyValue

-- | Usually environments will contain LazyValues.
type Environment = Environment' LazyValue

-- | And so will attribute sets.
type AttrSet = AttrSet' LazyValue

-- | The environment of most closures will be lazily evaluated.
type Closure = Closure' LazyValue

instance HasRTType Value LazyValue where
  typeOf (Value v) = typeOf v

-- | Wrap a value in a result.
validR :: Value -> LazyValue
validR = LazyValue . Result . Right

validR' :: Value' LazyValue -> LazyValue
validR' = validR . Value

-- | Shorthand for creating an Environment from a list.
mkEnv :: [(Text, Value)] -> Environment
mkEnv = Environment . H.fromList . map (map validR)

-- | Shorthand to create a closure from a list and an expression.
mkClosure :: [(Text, Value)] -> Expression -> Closure
mkClosure env expr = Closure (mkEnv env) expr

-- | Apply a native value as if it were a function, to zero or more arguments.
applyNative :: Native -> [LazyValue] -> LazyValue
applyNative native [] = unwrapNative native
applyNative native (rval:rvals) = case native of
  NativeFunction func -> applyNative (func rval) rvals
  NativeValue v -> expectedFunction v

-- | Turn a 'Native' into an 'LazyValue'. This might rewrap it in a
-- 'VNative' constructor, if the object is not a 'NativeValue'.
unwrapNative :: Native -> LazyValue
unwrapNative (NativeValue rv) = rv
unwrapNative n = validR $ Value $ VNative n

-- | Create a value from a string.
strV :: Text -> Value
strV = fromConstant . String

-- | Create a value from an integer.
intV :: Integer -> Value
intV = fromConstant . Int

-- | Create a value from a bool.
boolV :: Bool -> Value
boolV = fromConstant . Bool

-- | Create a null value.
nullV :: Value
nullV = fromConstant Null

-- | Create an attribute set value.
attrsV :: [(Text, Value)] -> Value
attrsV = Value . VAttrSet . mkEnv

-- | Create a list value.
listV :: [Value] -> Value
listV = Value . VList . fromList . map validR

-- | Create a function value.
functionV :: Text -> Closure -> Value
functionV param body = Value $ VFunction param body

-- | Create a native value.
nativeV :: Natify n => n -> Value
nativeV = Value . VNative . natify

-- | We can turn any lazy value into a native.
instance Natify LazyValue where
  natify = NativeValue

-- | This is where things get interesting: a 'Natify' instance for
-- functions on lazy values lets us embed any function on 'LazyValue's
-- as a 'Native' function. So for example, we can 'natify' a function
-- of type @'LazyValue' -> -- 'LazyValue'@, or @'LazyValue' ->
-- 'LazyValue' -> 'LazyValue'@, etc.
instance Natify t => Natify (LazyValue -> t) where
  natify function = NativeFunction $ \lval -> natify (function lval)

-- | We can 'natify' an arbitrary function on values (provided its
-- return type implements 'Natify'). However, it has the effect of
-- forcing strict evaluation, as the only way to extract the inner
-- value is to evaluate the 'Result' wrapper to WHNF.
instance (Natify a, Natify b) => Natify (a -> b) where
  natify function = NativeFunction $ \res -> do
    val <- natify res
    return $ natify $ function val

-------------------------------------------------------------------------------
--------------------------- Deep Evaluation -----------------------------------
-------------------------------------------------------------------------------

-- | Most of the time we evaluate things lazily, but sometimes we want
-- to be able to evaluate strictly (esp. in the `deepSeq` function).
deeplyEval :: Value -> Result Value
deeplyEval v@(Value val) = case val of
  VNative (NativeValue lval) -> deeplyEvalLazy lval
  VList lvals -> Value . VList <$> foldM step mempty lvals where
    step = undefined
    -- -- Recur on items of the list, and check if any are errors.
    -- let (_, errs) = break (isError . deeplyEvalLazy) lvals
    -- case toList errs of
    --   [] -> return v -- no errors, can just return the thing
    --   (err:_) -> errorR err -- return the first error found
  VAttrSet attrs -> Value . VAttrSet <$> foldM step emptyE (envToList attrs) where
    step = undefined
    -- let (_, errs) = break (isError . deeplyEvalLazy) $ H.elems lvals
    -- case errs of
    --   [] -> return v -- no errors, can just return the thing
    --   (err:_) -> _errorR err -- return the first error found
  -- Note that we don't recur into functions (or NativeFunctions), and
  -- since constants can't fail there's no need to handle them either.
  _ -> return v

-- | Deeply evaluate a lazy value, while keeping it appearing as lazy.
deeplyEvalLazy :: LazyValue -> Result Value
deeplyEvalLazy lval = case unLVal lval of
  err@(Result (Left _)) -> err
  Result (Right v) -> deeplyEval v

instance FromConstant LazyValue where
  fromConstant = validR . fromConstant
  fromConstants = validR . fromConstants
  fromConstantSet = validR . fromConstantSet

instance FromConstant Value where
  fromConstant = Value . fromConstant
  fromConstants = Value . fromConstants
  fromConstantSet = Value . fromConstantSet

-- | Tests if a lazy value is an error (forces WHNF evaluation)
isError :: Result Value -> Result Bool
isError (Result res) = case res of
  Left _ -> return True
  _ -> return False

-- | Synonym for monadic bind, applying a function inside of a
-- 'Result', provided the 'Result' contains a 'Value'.
unwrapAndApply :: (Value -> Result Value) -> LazyValue -> LazyValue
unwrapAndApply func (LazyValue res) = LazyValue $ do
  val :: Value <- res
  func val

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
module Nix.Eval.Builtins where

import Nix.Common
import Nix.Eval.Constants
import Nix.Eval.Expressions
import Nix.Eval.Values

-- | Turns a function that operates on an evaluated value, and makes
-- it lazy by delaying the inspection of whether the value is an
-- error or not until the last minute.
lazify :: (Value -> Result Value) -> BuiltinFunc
lazify function (Result res) = case res of
  Left err -> Result $ Left err
  Right val -> function val

-- | Same as @lazify@, but for binary functions.
lazify2 :: (Value -> Value -> Result Value) -> BuiltinFunc2
lazify2 function (Result res1) (Result res2) = case res1 of
  Left err -> Result $ Left err
  Right val1 -> case res2 of
    Left err -> Result $ Left err
    Right val2 -> function val1 val2

-- | A simple function which adds one to its argument.
addOne :: BuiltinFunc
addOne = lazify $ \case
  VConstant (Int n) -> pure $ VConstant $ Int (n + 1)
  v -> expectedInt v

reverseString :: BuiltinFunc
reverseString = lazify $ \case
  VConstant (String s) -> pure $ VConstant $ String $ reverse s
  v -> expectedString v

-- | Conversion to environment variables for constants.
constantToString :: Constant -> Text
constantToString (String s) = s
constantToString (Int i) = tshow i
constantToString (Path p) = pathToText p
constantToString (Bool True) = "1"
constantToString (Bool False) = ""
constantToString Null = ""

-- | Convert a value to a env-variable-compatible string.
valueToString :: Value -> Result Text
valueToString (VConstant c) = pure $ constantToString c
valueToString (VList vals) = do
  strings <- mapM valueToString vals
  pure $ intercalate " " $ toList strings
valueToString v = do
  let types = [RT_String, RT_Path, RT_Bool, RT_Int, RT_Null, RT_List]
  expectedOneOf types v

-- | Convert a value to a string.
bi_toString :: BuiltinFunc
bi_toString = lazify $ map strV . valueToString

bi_seq :: BuiltinFunc2
bi_seq (Result res1) res2 = seq res1 res2

-- | The throw function forces an error to occur.
bi_throw :: BuiltinFunc
bi_throw = lazify $ \case
  VConstant (String msg) -> throwCustom msg
  v -> expectedString v

-- | Implementation of binary addition.
bi_plus :: BuiltinFunc2
bi_plus = lazify2 plus where
  val1 `plus` val2 = case val1 of
    VConstant (String s) -> case val2 of
      VConstant (String s') -> pure $ strV $ s <> s'
      v -> expectedString v
    VConstant (Int i) -> case val2 of
      VConstant (Int i') -> pure $ intV $ i + i'
      v -> expectedInt v
    _ -> expectedOneOf [RT_String, RT_Int] val1

-- | Implementation of binary subtraction.
bi_minus :: BuiltinFunc2
bi_minus = lazify2 minus where
  val1 `minus` val2 = case val1 of
    VConstant (Int i) -> case val2 of
      VConstant (Int i') -> pure $ intV $ i - i'
      v -> expectedInt v
    _ -> expectedOneOf [RT_Int] val1

-- | Implementation of binary multiplication.
bi_times :: BuiltinFunc2
bi_times = lazify2 times where
  val1 `times` val2 = case val1 of
    VConstant (Int i) -> case val2 of
      VConstant (Int i') -> pure $ intV $ i * i'
      v -> expectedInt v
    _ -> expectedOneOf [RT_Int] val1

-- | Implementation of logical AND. This short-circuits, so we can't
-- just use the `lazify2` function.
bi_and :: BuiltinFunc2
bi_and (Result res1) (Result res2) = case res1 of
  Left err -> errorR err
  Right val1 -> case val1 of
    VConstant (Bool False) -> pure $ boolV False
    VConstant (Bool True) -> case res2 of
      Left err -> errorR err
      Right val2 -> case val2 of
        VConstant (Bool b) -> pure $ boolV b
        v -> expectedBool v
    _ -> expectedBool val1

-- | Implementation of logical OR. Also short-circuits.
bi_or :: BuiltinFunc2
bi_or (Result res1) (Result res2) = case res1 of
  Left err -> errorR err
  Right val1 -> case val1 of
    VConstant (Bool True) -> pure $ boolV True
    VConstant (Bool False) -> case res2 of
      Left err -> errorR err
      Right val2 -> case val2 of
        VConstant (Bool b) -> pure $ boolV b
        v -> expectedBool v
    _ -> expectedBool val1

interpretBinop :: BinaryOp -> BuiltinFunc2
interpretBinop BO_Plus = bi_plus
interpretBinop BO_And = bi_and
interpretBinop BO_Or = bi_or
interpretBinop BO_Minus = bi_minus
interpretBinop BO_Times = bi_times
interpretBinop b =
  error ("Binary operator " <> show b <> " is not implemented.")

-- | The set of built-in functions added to the environment before evaluation.
allBuiltins :: Environment
allBuiltins = mkEnv $
  [ ("addOne", VBuiltin "addOne" addOne)
  , ("reverseString", VBuiltin "reverseString" reverseString)]

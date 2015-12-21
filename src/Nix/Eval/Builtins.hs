{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
module Nix.Eval.Builtins where

import qualified Data.HashMap.Strict as H

import Nix.Common
import Nix.Eval.Constants
import Nix.Eval.Expressions
import Nix.Eval.Values

-- | Builtin functions take a value, and can either return an error or a
-- new value.
type BuiltinFunc = Value -> Result Value

-- | A simple function which adds one to its argument.
addOne :: BuiltinFunc
addOne (VConstant (Int n)) = pure $ VConstant $ Int (n + 1)
addOne v = expectedInt v

reverseString :: BuiltinFunc
reverseString (VConstant (String s)) = pure $ VConstant $ String $ reverse s
reverseString v = expectedString v

valueToString :: BuiltinFunc
valueToString (VConstant (String s)) = pure $ strV s
valueToString (VConstant (Path p)) = pure $ strV $ pathToText p
valueToString (VConstant (Bool True)) = pure $ strV "1"
valueToString (VConstant (Bool False)) = pure $ strV ""
valueToString v = expectedOneOf [RT_String, RT_Path, RT_Bool] v

-- | The throw function forces an error to occur.
throw :: BuiltinFunc
throw (VConstant (String msg)) = throwCustom msg
throw v = expectedString v

-- | Implementation of binary addition.
binop_plus :: Value -> BuiltinFunc
binop_plus val1 val2 = case val1 of
  VConstant (String s) -> case val2 of
    VConstant (String s') -> pure $ strV $ s <> s'
    v -> expectedString v
  VConstant (Int i) -> case val2 of
    VConstant (Int i') -> pure $ intV $ i + i'
    v -> expectedInt v
  _ -> expectedOneOf [RT_String, RT_Int] val1

-- | Implementation of logical AND.
binop_and :: Value -> BuiltinFunc
binop_and val1 val2 = case val1 of
  VConstant (Bool False) -> pure $ boolV False
  VConstant (Bool True) -> case val2 of
    VConstant (Bool b) -> pure $ boolV b
    v -> expectedBool v
  _ -> expectedBool val1


-- | Implementation of logical OR.
binop_or :: Value -> BuiltinFunc
binop_or val1 val2 = case val1 of
  VConstant (Bool True) -> pure $ boolV True
  VConstant (Bool False) -> case val2 of
    VConstant (Bool b) -> pure $ boolV b
    v -> expectedBool v
  _ -> expectedBool val1

interpretBinop :: BinaryOp -> Value -> BuiltinFunc
interpretBinop BO_Plus = binop_plus
interpretBinop BO_And = binop_and
interpretBinop BO_Or = binop_or
interpretBinop b =
  error ("Binary operator " <> show b <> " is not implemented.")

-- | The set of built-in functions added to the environment before evaluation.
allBuiltins :: Environment
allBuiltins = Environment $ H.fromList
  [ ("addOne", VBuiltin "addOne" addOne)
  , ("reverseString", VBuiltin "reverseString" reverseString)]

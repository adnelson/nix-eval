module Nix.Eval.Operators where

import Nix.Common
import Nix.Types (NBinaryOp(..), NUnaryOp(..))
import Nix.Eval.Constants
import Nix.Eval.Expressions
import Nix.Eval.Values

binop_concat :: Native
binop_concat = natify $ \val1 val2 -> case val1 of
  VList list1 -> case val2 of
    VList list2 -> validR $ VList $ list1 <> list2
    v -> expectedList v
  _ -> expectedList val1

-- | Implementation of binary addition.
binop_plus :: Native
binop_plus = natify plus where
  val1 `plus` val2 = case val1 of
    VConstant (String s) -> case val2 of
      VConstant (String s') -> validR $ strV $ s <> s'
      v -> expectedString v
    VConstant (Int i) -> case val2 of
      VConstant (Int i') -> validR $ intV $ i + i'
      v -> expectedInt v
    _ -> expectedOneOf [RT_String, RT_Int] val1

-- | Implementation of binary subtraction.
binop_minus :: Native
binop_minus = natify minus where
  val1 `minus` val2 = case val1 of
    VConstant (Int i) -> case val2 of
      VConstant (Int i') -> validR $ intV $ i - i'
      v -> expectedInt v
    _ -> expectedOneOf [RT_Int] val1

-- | Implementation of binary multiplication.
binop_times :: Native
binop_times = natify times where
  val1 `times` val2 = case val1 of
    VConstant (Int i) -> case val2 of
      VConstant (Int i') -> validR $ intV $ i * i'
      v -> expectedInt v
    _ -> expectedInt val1

-- | Implementation of logical AND.
binop_and :: Native
binop_and = natify $ \val1 -> case val1 of
  VConstant (Bool False) -> \_ -> validR $ boolV False
  VConstant (Bool _) -> unwrapAndApply $ \case
    VConstant (Bool b) -> validR $ boolV b
    v -> expectedBool v
  _ -> \_ -> expectedBool val1

-- | Implementation of logical OR.
binop_or :: Native
binop_or = natify $ \val1 -> case val1 of
  VConstant (Bool True) -> \_ -> validR $ boolV True
  VConstant (Bool _) -> unwrapAndApply $ \case
    VConstant (Bool b) -> validR $ boolV b
    v -> expectedBool v
  _ -> \_ -> expectedBool val1

-- | Builtin division function; prevents divide-by-zero
binop_div :: Native
binop_div = natify $ \v1 v2 -> case (v1, v2) of
  (VConstant (Int _), VConstant (Int 0)) -> errorR DivideByZero
  (VConstant (Int i), VConstant (Int j)) -> validR $ intV (i `div` j)
  (v, _) -> expectedInt v

interpretBinop :: NBinaryOp -> Native
interpretBinop NPlus = binop_plus
interpretBinop NAnd = binop_and
interpretBinop NOr = binop_or
interpretBinop NMinus = binop_minus
interpretBinop NMult = binop_times
interpretBinop NConcat = binop_concat
interpretBinop b =
  error ("Binary operator " <> show b <> " is not yet implemented.")

module Nix.Eval.Operators where

import Nix.Common
import Nix.Types (NBinaryOp(..), NUnaryOp(..))
import Nix.Eval.Constants
import Nix.Eval.Expressions
import Nix.Eval.Values

-- | Concatenation of lists.
binop_concat :: Value -> Value -> LazyValue
binop_concat val1 val2 = case unVal val1 of
  VList list1 -> case unVal val2 of
    VList list2 -> validR $ Value $ VList $ list1 <> list2
    _ -> expectedList val2
  _ -> expectedList val1

-- | Implementation of binary addition.
binop_plus :: Value -> Value -> LazyValue
binop_plus val1 val2 = case unVal val1 of
  VConstant (String s) -> case unVal val2 of
    VConstant (String s') -> convert $ s <> s'
    _ -> expectedString val2
  VConstant (Int i) -> case unVal val2 of
    VConstant (Int i') -> convert $ i + i'
    _ -> expectedInt val2
  _ -> expectedOneOf [RT_String, RT_Int] val1

-- | Generates a binary operation on integers.
mkBinopNum :: ToConstant t => (Integer -> Integer -> t)
           -> (Value -> Value -> LazyValue)
mkBinopNum op val1 val2 = case unVal val1 of
  VConstant (Int i) -> case unVal val2 of
    VConstant (Int i') -> convert $ op i i'
    _ -> expectedInt val2
  _ -> expectedInt val1

-- | Implementation of logical AND. Short-circuits.
binop_and :: Value -> LazyValue -> LazyValue
binop_and val = case unVal val of
  VConstant (Bool False) -> \_ -> validR $ boolV False
  VConstant (Bool _) -> unwrapAndApply $ \v -> case unVal v of
    VConstant (Bool b) -> convert b
    _ -> expectedBool v
  _ -> \_ -> expectedBool val

-- | Implementation of logical OR. Short-circuits.
binop_or :: Value -> LazyValue -> LazyValue
binop_or val = case unVal val of
  VConstant (Bool True) -> \_ -> validR $ boolV True
  VConstant (Bool _) -> unwrapAndApply $ \v -> case unVal v of
    VConstant (Bool b) -> convert b
    _ -> expectedBool v
  _ -> \_ -> expectedBool val

-- | Implementation of logical implication. Short-circuits.
binop_impl :: Value -> LazyValue -> LazyValue
binop_impl val = case unVal val of
  VConstant (Bool False) -> \_ -> validR $ boolV True
  VConstant (Bool _) -> unwrapAndApply $ \v -> case unVal v of
    VConstant (Bool b) -> convert b
    _ -> expectedBool v
  _ -> \_ -> expectedBool val

-- | Builtin division function; prevents divide-by-zero
binop_div :: Value -> Value -> LazyValue
binop_div val1 val2 = case (unVal val1, unVal val2) of
  (VConstant (Int _), VConstant (Int 0)) -> errorR DivideByZero
  (VConstant (Int i), VConstant (Int j)) -> validR $ intV (i `div` j)
  (_, VConstant (Int _)) -> expectedInt val1
  (VConstant (Int _), _) -> expectedInt val2
  (_, _) -> expectedInt val1

-- | Adds all of the keys from the second set into the first.
binop_update :: Value -> LazyValue -> LazyValue
binop_update val = case unVal val of
  VAttrSet set1 -> unwrapAndApply $ \v -> case unVal v of
    VAttrSet set2 -> validR $ Value $ VAttrSet (set2 `unionEnv` set1)
    _ -> expectedAttrs v
  _ -> \_ -> expectedAttrs val

-- | Equality of values.
binop_eq :: Value -> Value -> LazyValue
binop_eq v1 v2 = fromBool (v1 == v2)

-- | Inequality of values.
binop_neq :: Value -> Value -> LazyValue
binop_neq v1 v2 = fromBool (v1 /= v2)

-- | Translate a binary operator into a native (binary) function.
interpretBinop :: NBinaryOp -> Native
interpretBinop NEq = natify binop_eq
interpretBinop NNEq = natify binop_neq
interpretBinop NLt = natify $ mkBinopNum (<)
interpretBinop NLte = natify $ mkBinopNum (<=)
interpretBinop NGt = natify $ mkBinopNum (>)
interpretBinop NGte = natify $ mkBinopNum (>=)
interpretBinop NAnd = natify binop_and
interpretBinop NOr = natify binop_or
interpretBinop NImpl = natify binop_impl
interpretBinop NUpdate = natify binop_update
interpretBinop NPlus = natify binop_plus
interpretBinop NMinus = natify $ mkBinopNum (-)
interpretBinop NMult = natify $ mkBinopNum (*)
interpretBinop NDiv = natify binop_div
interpretBinop NConcat = natify binop_concat

unop_not :: Value -> LazyValue
unop_not v = case unVal v of
  (VConstant (Bool b)) -> convert $ not b
  _ -> expectedBool v

unop_neg :: Value -> LazyValue
unop_neg val = case unVal val of
  (VConstant (Int i)) -> convert (-i)
  _ -> expectedInt val

interpretUnop :: NUnaryOp -> Native
interpretUnop NNeg = natify unop_neg
interpretUnop NNot = natify unop_not

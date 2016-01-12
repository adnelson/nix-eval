module Nix.Eval.Values.Builtins.Operators where

import Nix.Common
import Nix.Types (NBinaryOp(..), NUnaryOp(..))
import Nix.Eval.Constants
import Nix.Eval.Expressions
import Nix.Eval.Values
import Nix.Eval.Values.NativeConversion

-- | Concatenation of lists.
binop_concat :: WHNFValue -> WHNFValue -> LazyValue
binop_concat val1 val2 = case val1 of
  VList list1 -> case val2 of
    VList list2 -> pure $ VList $ list1 <> list2
    _ -> expectedList val2
  _ -> expectedList val1

-- | Implementation of binary addition.
binop_plus :: WHNFValue -> WHNFValue -> LazyValue
binop_plus val1 val2 = case val1 of
  VConstant (String s) -> case val2 of
    VConstant (String s') -> convert $ s <> s'
    _ -> expectedString val2
  VConstant (Int i) -> case val2 of
    VConstant (Int i') -> convert $ i + i'
    _ -> expectedInt val2
  _ -> expectedOneOf [RT_String, RT_Int] val1

-- | Generates a binary operation on integers.
mkBinopNum :: ToConstant t => (Integer -> Integer -> t)
           -> (WHNFValue -> WHNFValue -> LazyValue)
mkBinopNum op val1 val2 = case val1 of
  VConstant (Int i) -> case val2 of
    VConstant (Int i') -> convert $ op i i'
    _ -> expectedInt val2
  _ -> expectedInt val1

-- | Implementation of logical AND. Short-circuits.
binop_and :: WHNFValue -> LazyValue -> LazyValue
binop_and val = case val of
  VConstant (Bool False) -> \_ -> pure $ boolV False
  VConstant (Bool _) -> unwrapAndApply $ \v -> case v of
    VConstant (Bool b) -> convert b
    _ -> expectedBool v
  _ -> \_ -> expectedBool val

-- | Implementation of logical OR. Short-circuits.
binop_or :: WHNFValue -> LazyValue -> LazyValue
binop_or val = case val of
  VConstant (Bool True) -> \_ -> pure $ boolV True
  VConstant (Bool _) -> unwrapAndApply $ \v -> case v of
    VConstant (Bool b) -> convert b
    _ -> expectedBool v
  _ -> \_ -> expectedBool val

-- | Implementation of logical implication. Short-circuits.
binop_impl :: WHNFValue -> LazyValue -> LazyValue
binop_impl val = case val of
  VConstant (Bool False) -> \_ -> pure $ boolV True
  VConstant (Bool _) -> unwrapAndApply $ \v -> case v of
    VConstant (Bool b) -> convert b
    _ -> expectedBool v
  _ -> \_ -> expectedBool val

-- | Builtin division function; prevents divide-by-zero
binop_div :: WHNFValue -> WHNFValue -> LazyValue
binop_div val1 val2 = case (val1, val2) of
  (VConstant (Int _), VConstant (Int 0)) -> throwError DivideByZero
  (VConstant (Int i), VConstant (Int j)) -> pure $ intV (i `div` j)
  (_, VConstant (Int _)) -> expectedInt val1
  (VConstant (Int _), _) -> expectedInt val2
  (_, _) -> expectedInt val1

-- | Adds all of the keys from the second set into the first.
binop_update :: WHNFValue -> LazyValue -> LazyValue
binop_update val = case val of
  VAttrSet set1 -> unwrapAndApply $ \v -> case v of
    VAttrSet set2 -> pure $ VAttrSet (set2 `unionEnv` set1)
    _ -> expectedAttrs v
  _ -> \_ -> expectedAttrs val

-- | Equality of values.
binop_eq :: WHNFValue -> WHNFValue -> LazyValue
binop_eq v1 v2 = undefined -- fromBool (v1 == v2)

-- | Inequality of values.
binop_neq :: WHNFValue -> WHNFValue -> LazyValue
binop_neq v1 v2 = undefined -- fromBool (v1 /= v2)

-- | Translate a binary operator into a native (binary) function.
interpretBinop :: NBinaryOp -> LNative (WHNFValue -> WHNFValue -> WHNFValue)
-- interpretBinop NEq = natify binop_eq
-- interpretBinop NNEq = natify binop_neq
-- interpretBinop NLt = natify $ mkBinopNum (<)
-- interpretBinop NLte = natify $ mkBinopNum (<=)
-- interpretBinop NGt = natify $ mkBinopNum (>)
-- interpretBinop NGte = natify $ mkBinopNum (>=)
interpretBinop NAnd = toNative2L binop_and
-- interpretBinop NOr = natify binop_or
-- interpretBinop NImpl = natify binop_impl
interpretBinop NUpdate = toNative2L binop_update
-- interpretBinop NPlus = natify binop_plus
-- interpretBinop NMinus = natify $ mkBinopNum (-)
-- interpretBinop NMult = natify $ mkBinopNum (*)
-- interpretBinop NDiv = natify binop_div
-- interpretBinop NConcat = natify binop_concat
interpretBinop _ = undefined

unop_not :: WHNFValue -> LazyValue
unop_not val = case val of
  (VConstant (Bool b)) -> convert $ not b
  _ -> expectedBool val

unop_neg :: WHNFValue -> LazyValue
unop_neg val = case val of
  (VConstant (Int i)) -> convert (-i)
  _ -> expectedInt val

interpretUnop :: NUnaryOp -> LNative (WHNFValue -> WHNFValue)
interpretUnop NNeg = toNative1 unop_neg
interpretUnop NNot = toNative1 unop_not

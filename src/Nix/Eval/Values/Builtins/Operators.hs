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

-- | Equality of values. Forces evaluation for lists and sets.
binop_eq :: WHNFValue -> WHNFValue -> LazyValue
binop_eq val1 val2 = case (val1, val2) of
  (VConstant c1, VConstant c2) -> convert $ c1 == c2
  (VAttrSet set1, VAttrSet set2)
    | envSize set1 /= envSize set2 -> convert False
    | otherwise -> go (envToList set1) where
        go [] = convert True
        go ((k, lval1):pairs) = case lookupEnv k set2 of
          Nothing -> convert False
          Just lval2 -> do
            val1 <- lval1
            val2 <- lval2
            binop_eq val1 val2 >>= \case
              VConstant (Bool True) -> go pairs
              _ -> convert False
  (VList list1, VList list2)
    | length list1 /= length list2 -> convert False
    | otherwise -> go (toList list1) (toList list2) where
        go [] [] = convert True
        go [] (_:_) = convert False
        go (_:_) [] = convert False
        go (lval1:lvals1) (lval2:lvals2) = do
          val1 <- lval1
          val2 <- lval2
          binop_eq val1 val2 >>= \case
            VConstant (Bool True) -> go lvals1 lvals2
            _ -> convert False
  (VNative (NativeValue lval1), VNative (NativeValue lval2)) -> do
    val1 <- lval1
    val2 <- lval2
    binop_eq val1 val2
  _ -> convert False

-- | Inequality of values.
binop_neq :: WHNFValue -> WHNFValue -> LazyValue
binop_neq v1 v2 = binop_eq v1 v2 >>= \case
  VConstant (Bool False) -> convert True
  _ -> convert False

-- | Translate a binary operator into a native (binary) function.
interpretBinop :: NBinaryOp -> LNative (WHNFValue -> WHNFValue -> WHNFValue)
interpretBinop NEq = toNative2 binop_eq
interpretBinop NNEq = toNative2 binop_neq
interpretBinop NLt = toNative2 $ mkBinopNum (<)
interpretBinop NLte = toNative2 $ mkBinopNum (<=)
interpretBinop NGt = toNative2 $ mkBinopNum (>)
interpretBinop NGte = toNative2 $ mkBinopNum (>=)
interpretBinop NAnd = toNative2L binop_and
interpretBinop NOr = toNative2L binop_or
interpretBinop NImpl = toNative2L binop_impl
interpretBinop NUpdate = toNative2L binop_update
interpretBinop NPlus = toNative2 binop_plus
interpretBinop NMinus = toNative2 $ mkBinopNum (-)
interpretBinop NMult = toNative2 $ mkBinopNum (*)
interpretBinop NDiv = toNative2 binop_div
interpretBinop NConcat = toNative2 binop_concat

unop_not :: WHNFValue -> LazyValue
unop_not val = case val of
  VConstant (Bool b) -> convert $ not b
  _ -> expectedBool val

unop_neg :: WHNFValue -> LazyValue
unop_neg val = case val of
  VConstant (Int i) -> convert (-i)
  _ -> expectedInt val

interpretUnop :: NUnaryOp -> LNative (WHNFValue -> WHNFValue)
interpretUnop NNeg = toNative1 unop_neg
interpretUnop NNot = toNative1 unop_not

module Nix.Eval.Builtins where

import Nix.Common
import Nix.Types (NBinaryOp(..), NUnaryOp(..))
import Nix.Eval.Constants
import Nix.Eval.Expressions
import Nix.Eval.Values

import qualified Data.Set as S

-- | Conversion to environment variables for constants.
constantToString :: Constant -> Text
constantToString (String s) = s
constantToString (Int i) = tshow i
constantToString (Path p) = pathToText p
constantToString (Bool True) = "1"
constantToString (Bool False) = ""
constantToString Null = ""

-- | Convert a value to a env-variable-compatible string.
valueToString :: Value -> Either EvalError Text
valueToString (VConstant c) = pure $ constantToString c
valueToString (VList vals) = do
  strings <- forM vals $ \(Result res) -> case res of
    Left err -> Left err
    Right val -> valueToString val
  pure $ intercalate " " $ toList strings
valueToString v = do
  let types = [RT_String, RT_Path, RT_Bool, RT_Int, RT_Null, RT_List]
  Left $ TypeError (S.fromList types) (typeOf v)

-- | Convert a value to a string.
bi_toString :: Native
bi_toString = natify $ \v -> case valueToString v of
  Left err -> errorR err
  Right str -> validR $ strV str

-- | Strict sequencing function.
bi_seq :: Native
bi_seq = NativeFunction $ \(Result res1) ->
  NativeFunction $ \res2 -> NativeValue (seq res1 res2)

-- | The throw function forces an error to occur.
bi_throw :: Native
bi_throw = natify $ \case
  VConstant (String msg) -> errorR $ CustomError msg
  v -> expectedString v

-- | Implementation of binary addition.
bi_plus :: Native
bi_plus = natify plus where
  val1 `plus` val2 = case val1 of
    VConstant (String s) -> case val2 of
      VConstant (String s') -> validR $ strV $ s <> s'
      v -> expectedString v
    VConstant (Int i) -> case val2 of
      VConstant (Int i') -> validR $ intV $ i + i'
      v -> expectedInt v
    _ -> expectedOneOf [RT_String, RT_Int] val1

-- | Implementation of binary subtraction.
bi_minus :: Native
bi_minus = natify minus where
  val1 `minus` val2 = case val1 of
    VConstant (Int i) -> case val2 of
      VConstant (Int i') -> validR $ intV $ i - i'
      v -> expectedInt v
    _ -> expectedOneOf [RT_Int] val1

-- | Implementation of binary multiplication.
bi_times :: Native
bi_times = natify times where
  val1 `times` val2 = case val1 of
    VConstant (Int i) -> case val2 of
      VConstant (Int i') -> validR $ intV $ i * i'
      v -> expectedInt v
    _ -> expectedInt val1

-- | Implementation of logical AND.
bi_and :: Native
bi_and = natify $ \val1 -> case val1 of
  VConstant (Bool False) -> \_ -> validR $ boolV False
  VConstant (Bool _) -> unwrapAndApply $ \case
    VConstant (Bool b) -> validR $ boolV b
    v -> expectedBool v
  _ -> \_ -> expectedBool val1

-- | Implementation of logical OR.
bi_or :: Native
bi_or = natify $ \val1 -> case val1 of
  VConstant (Bool True) -> \_ -> validR $ boolV True
  VConstant (Bool _) -> unwrapAndApply $ \case
    VConstant (Bool b) -> validR $ boolV b
    v -> expectedBool v
  _ -> \_ -> expectedBool val1

-- | Builtin division function; prevents divide-by-zero
bi_div :: Native
bi_div = natify $ \v1 v2 -> case (v1, v2) of
  (VConstant (Int _), VConstant (Int 0)) -> errorR DivideByZero
  (VConstant (Int i), VConstant (Int j)) -> validR $ intV (i `div` j)
  (v, _) -> expectedInt v

-- | Asserts its first argument is true, and then returns its second.
bi_assert :: Native
bi_assert = natify $ \val res -> case val of
  VConstant (Bool True) -> res
  VConstant (Bool False) -> errorR AssertionError
  v -> expectedBool v

bi_concat :: Native
bi_concat = natify $ \val1 val2 -> case val1 of
  VList list1 -> case val2 of
    VList list2 -> validR $ VList $ list1 <> list2
    v -> expectedList v
  _ -> expectedList val1


interpretBinop :: NBinaryOp -> Native
interpretBinop NPlus = bi_plus
interpretBinop NAnd = bi_and
interpretBinop NOr = bi_or
interpretBinop NMinus = bi_minus
interpretBinop NMult = bi_times
interpretBinop NConcat = bi_concat
interpretBinop b =
  error ("Binary operator " <> show b <> " is not yet implemented.")

-- | The set of built-in functions to add to the environment before
-- evaluation.
allBuiltins :: AttrSet
allBuiltins = mkEnv [("throw", VNative bi_throw)]

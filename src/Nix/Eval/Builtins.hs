module Nix.Eval.Builtins where

import Nix.Common
import Nix.Types (NBinaryOp(..), NUnaryOp(..))
import Nix.Eval.Constants
import Nix.Eval.Expressions
import Nix.Eval.Values

import qualified Data.Set as S

-- | Conversion to environment variables for constants.
constantToEnvString :: Constant -> Text
constantToEnvString (String s) = s
constantToEnvString (Int i) = tshow i
constantToEnvString (Path p) = pathToText p
constantToEnvString (Bool True) = "1"
constantToEnvString (Bool False) = ""
constantToEnvString Null = ""

-- | Convert a value to a env-variable-compatible string.
valueToEnvString :: Value -> Either EvalError Text
valueToEnvString (VConstant c) = pure $ constantToEnvString c
valueToEnvString (VList vals) = do
  strings <- forM vals $ \(Result res) -> case res of
    Left err -> Left err
    Right val -> valueToEnvString val
  pure $ intercalate " " $ toList strings
valueToEnvString v = do
  let types = [RT_String, RT_Path, RT_Bool, RT_Int, RT_Null, RT_List]
  Left $ TypeError (S.fromList types) (typeOf v)

-- | Convert a value to a string.
builtin_toString :: Native
builtin_toString = natify $ \v -> case valueToEnvString v of
  Left err -> errorR err
  Right str -> validR $ strV str

-- | Strict sequencing function.
builtin_seq :: Native
builtin_seq = natify $ \(Result res1) res2 -> case res1 of
  Left err -> errorR err
  Right (val :: Value) -> seq val res2

-- | The throw function forces an error to occur.
builtin_throw :: Native
builtin_throw = natify $ \case
  VConstant (String msg) -> errorR $ CustomError msg
  v -> expectedString v

-- | Asserts its first argument is true, and then returns its second.
builtin_assert :: Native
builtin_assert = natify $ \val res -> case val of
  VConstant (Bool True) -> res
  VConstant (Bool False) -> errorR AssertionError
  v -> expectedBool v

builtin_length :: Native
builtin_length = natify $ \case
  VList vals -> validR $ fromInt (length vals)
  v -> expectedList v

-- | The set of built-in functions to add to the environment before
-- evaluation.
builtins :: AttrSet
builtins = mkEnv [("throw", VNative builtin_throw),
                  ("length", VNative builtin_length)]

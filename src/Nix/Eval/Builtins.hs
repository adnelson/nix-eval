module Nix.Eval.Builtins where

import Nix.Common
import Nix.Types (NBinaryOp(..), NUnaryOp(..))
import Nix.Eval.Constants
import Nix.Eval.Expressions
import Nix.Eval.Values

import qualified Data.Set as S
import qualified Data.HashMap.Strict as H

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
builtin_toString :: Value -> LazyValue
builtin_toString val = case valueToEnvString val of
  Left err -> errorR err
  Right str -> validR $ strV str

builtin_seq :: Value -> LazyValue -> LazyValue
builtin_seq = seq

-- | The throw function forces an error to occur.
builtin_throw :: Value -> LazyValue
builtin_throw = \case
  VConstant (String msg) -> errorR $ CustomError msg
  v -> expectedString v

-- | Asserts its first argument is true, and then returns its second.
builtin_assert :: Value -> LazyValue -> LazyValue
builtin_assert val res = case val of
  VConstant (Bool True) -> res
  VConstant (Bool False) -> errorR AssertionError
  v -> expectedBool v

builtin_length :: Value -> LazyValue
builtin_length = \case
  VList vals -> validR $ fromInt (length vals)
  v -> expectedList v

mkTypeTest :: RuntimeType -> Value -> LazyValue
mkTypeTest type_ = convert . hasType type_

-- | A bunch of runtime type checking tests.
builtin_isAttrs, builtin_isList, builtin_isFunction, builtin_isInt,
  builtin_isBool, builtin_isNull :: Value -> LazyValue
builtin_isAttrs = mkTypeTest RT_AttrSet
builtin_isList = mkTypeTest RT_List
builtin_isFunction = mkTypeTest RT_Function
builtin_isInt = mkTypeTest RT_Int
builtin_isBool = mkTypeTest RT_Bool
builtin_isNull = mkTypeTest RT_Null

-- | Deeply evaluate the first argument, and return the second if it's
-- the first has no errors; else error.
builtin_deepSeq :: Value -> LazyValue -> LazyValue
builtin_deepSeq val = case deeplyEval val of
  err@(Result (Left _)) -> \_ -> err
  _ -> id

-- | The set of built-in functions to add to the environment before
-- evaluation.
builtins :: AttrSet
builtins = mkEnv [ ("throw", nativeV builtin_throw)
                 , ("seq", nativeV builtin_seq)
                 , ("length", nativeV builtin_length)
                 , ("isAttrs", nativeV builtin_length)
                 , ("isList", nativeV builtin_length)
                 , ("isFunction", nativeV builtin_length)
                 , ("isBool", nativeV builtin_length)
                 , ("length", nativeV builtin_length)
                 ]

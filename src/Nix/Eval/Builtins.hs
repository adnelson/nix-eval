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
valueToEnvString :: Value -> Result Text
valueToEnvString val = case unVal val of
  VConstant c -> pure $ constantToEnvString c
  VList vals -> do
    strings <- forM vals $ \lazyVal -> do
      val <- lazyVal
      valueToEnvString val
    pure $ intercalate " " $ toList strings
  _ -> do
    let types = [RT_String, RT_Path, RT_Bool, RT_Int, RT_Null, RT_List]
    expectedOneOf types val

-- | Convert a value to a string.
builtin_toString :: Value -> LazyValue
builtin_toString val = map strV $ valueToEnvString val

builtin_seq :: Value -> LazyValue -> LazyValue
builtin_seq = seq

-- | The throw function forces an error to occur.
builtin_throw :: Value -> LazyValue
builtin_throw val = case unVal val of
  VConstant (String msg) -> errorR $ CustomError msg
  _ -> expectedString val

-- | Asserts its first argument is true, and then returns its second.
builtin_assert :: Value -> LazyValue -> LazyValue
builtin_assert val res = case unVal val of
  VConstant (Bool True) -> res
  VConstant (Bool False) -> errorR AssertionError
  _ -> expectedBool val

-- | Get the length of a list.
builtin_length :: Value -> LazyValue
builtin_length val = case unVal val of
  VList vals -> validR $ fromInt (length vals)
  v -> expectedList v

-- | Add to the front of a list.
builtin_cons :: LazyValue -> Value -> LazyValue
builtin_cons val v = case unVal v of
  VList list -> validR $ Value $ VList $ (val `cons` list)
  _ -> expectedList v

-- | Index into list. The list is the first argument.
builtin_elemAt :: Value -> Value -> LazyValue
builtin_elemAt val1 val2 = case unVal val1 of
  VList list -> case unVal val2 of
    VConstant (Int i) | i < 0 -> errorR $ IndexError i (length list)
    VConstant (Int i) -> case list `index` fromIntegral i of
      Nothing -> errorR $ IndexError i (length list)
      Just val -> val
    _ -> expectedInt val2
  _ -> expectedList val1

-- | Get the head of a list.
builtin_head :: Value -> LazyValue
builtin_head val@(Value v) = case v of
  VList _ -> builtin_elemAt val (fromInt 0)
  _ -> expectedList val

-- | Creates an `isX` function given a type to test a value against.
mkTypeTest :: RuntimeType -> Value -> LazyValue
mkTypeTest type_ = map convert . hasType type_

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

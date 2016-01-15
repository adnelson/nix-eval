module Nix.Eval.Builtins.NativeFunctions where

import Nix.Common
import Nix.Types (NBinaryOp(..), NUnaryOp(..))
import Nix.Constants
import Nix.Expressions
import Nix.Eval.Evaluator (evalApply)
import Nix.Values
import Nix.Values.NativeConversion

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
valueToEnvString :: WHNFValue -> Eval Text
valueToEnvString val = case val of
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
builtin_toString :: WHNFValue -> LazyValue
builtin_toString val = map strV $ valueToEnvString val

builtin_seq :: WHNFValue -> LazyValue -> LazyValue
builtin_seq = seq

-- | The throw function forces an error to occur.
builtin_throw :: WHNFValue -> LazyValue
builtin_throw val = case val of
  VConstant (String msg) -> throwError $ CustomError msg
  _ -> expectedString val

-- | Asserts its first argument is true, and then returns its second.
builtin_assert :: WHNFValue -> LazyValue -> LazyValue
builtin_assert val res = case val of
  VConstant (Bool True) -> res
  VConstant (Bool False) -> throwError AssertionError
  _ -> expectedBool val

-- | Get the length of a list.
builtin_length :: WHNFValue -> LazyValue
builtin_length val = case val of
  VList vals -> pure $ fromInt (length vals)
  v -> expectedList v

-- | Add to the front of a list.
builtin_cons :: LazyValue -> WHNFValue -> LazyValue
builtin_cons val v = case v of
  VList list -> pure $ VList $ (val `cons` list)
  _ -> expectedList v

-- | Index into list. The list is the first argument.
builtin_elemAt :: WHNFValue -> WHNFValue -> LazyValue
builtin_elemAt val1 val2 = case val1 of
  VList list -> case val2 of
    VConstant (Int i) | i < 0 -> throwError $ IndexError i (length list)
    VConstant (Int i) -> case list `index` fromIntegral i of
      Nothing -> throwError $ IndexError i (length list)
      Just val -> val
    _ -> expectedInt val2
  _ -> expectedList val1

-- | Get the head of a list.
builtin_head :: WHNFValue -> LazyValue
builtin_head val = case val of
  VList list -> case uncons list of
    Nothing -> throwError EmptyList
    Just (first, _) -> first
  _ -> expectedList val

-- | Get the tail of a list.
builtin_tail :: WHNFValue -> LazyValue
builtin_tail val = case val of
  VList list -> case uncons list of
    Nothing -> throwError EmptyList
    Just (_, rest) -> pure $ VList rest
  _ -> expectedList val

-- | Maps a function over a list.
builtin_map :: LazyValue -> WHNFValue -> LazyValue
builtin_map func = \case
  VList list -> pure $ VList $ map (evalApply func) list
  val -> expectedList val

-- | Creates an `isX` function given a type to test a value against.
mkTypeTest :: RuntimeType -> WHNFValue -> LazyValue
mkTypeTest type_ = map convert . hasType type_

-- | A bunch of runtime type checking tests.
builtin_isAttrs, builtin_isList, builtin_isFunction, builtin_isInt,
  builtin_isBool, builtin_isNull, builtin_isString, builtin_isPath
  :: WHNFValue -> LazyValue
builtin_isAttrs = mkTypeTest RT_Set
builtin_isList = mkTypeTest RT_List
builtin_isFunction = mkTypeTest RT_Lambda
builtin_isInt = mkTypeTest RT_Int
builtin_isBool = mkTypeTest RT_Bool
builtin_isNull = mkTypeTest RT_Null
builtin_isString = mkTypeTest RT_String
builtin_isPath = mkTypeTest RT_Path

-- | Deeply evaluate the first argument, and return the second if it's
-- the first has no errors; else error.
builtin_deepSeq :: WHNFValue -> LazyValue -> LazyValue
builtin_deepSeq val x = deeplyEval val >> x

-- | Get the type of a value as a string.
builtin_typeOf :: WHNFValue -> LazyValue
builtin_typeOf v = convert . typeToString <$> typeOf v

builtin_stringLength :: WHNFValue -> LazyValue
builtin_stringLength = \case
  VConstant (String s) -> convert (length s)
  v -> expectedString v

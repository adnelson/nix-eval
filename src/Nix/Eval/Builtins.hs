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

-- | Get the length of a list.
builtin_length :: Value -> LazyValue
builtin_length = \case
  VList vals -> validR $ fromInt (length vals)
  v -> expectedList v

-- | Add to the front of a list.
builtin_cons :: LazyValue -> Value -> LazyValue
builtin_cons val = \case
  VList list -> validR $ VList $ (val `cons` list)
  v -> expectedList v

-- | Index into list. The list is the first argument.
builtin_elemAt :: Value -> Value -> LazyValue
builtin_elemAt val1 val2 = case val1 of
  VList list -> case val2 of
    VConstant (Int i) | i < 0 -> errorR $ IndexError i (length list)
    VConstant (Int i) -> case list `index` fromIntegral i of
      Nothing -> errorR $ IndexError i (length list)
      Just val -> val
    v -> expectedInt v
  _ -> expectedList val1

-- | Get the head of a list.
builtin_head :: Value -> LazyValue
builtin_head val = case val of
  VList _ -> builtin_elemAt val (fromInt 0)
  _ -> expectedList val

-- | Creates an `isX` function given a type to test a value against.
mkTypeTest :: RuntimeType -> Value -> LazyValue
mkTypeTest type_ = convert . hasType type_

-- | A bunch of runtime type checking tests.
builtin_isAttrs, builtin_isList, builtin_isFunction, builtin_isInt,
  builtin_isString, builtin_isBool, builtin_isNull :: Value -> LazyValue
builtin_isAttrs = mkTypeTest RT_AttrSet
builtin_isList = mkTypeTest RT_List
builtin_isFunction = mkTypeTest RT_Function
builtin_isInt = mkTypeTest RT_Int
builtin_isString = mkTypeTest RT_String
builtin_isBool = mkTypeTest RT_Bool
builtin_isNull = mkTypeTest RT_Null

-- | Deeply evaluate the first argument, and return the second if it's
-- the first has no errors; else error.
builtin_deepSeq :: Value -> LazyValue -> LazyValue
builtin_deepSeq val = case deeplyEval val of
  err@(Result (Left _)) -> \_ -> err
  _ -> id

-- | Throws a 'NotImplemented' error with the given name.
notImplemented :: Text -> LazyValue
notImplemented name = errorR (NotImplemented name)

-- | The `builtins` object.
builtins :: AttrSet
builtins = mkEnv
  [ ("add", nativeV $ notImplemented "add")
  , ("all", nativeV $ notImplemented "all")
  , ("any", nativeV $ notImplemented "any")
  , ("attrNames", nativeV $ notImplemented "attrNames")
  , ("attrValues", nativeV $ notImplemented "attrValues")
  , ("compareVersions", nativeV $ notImplemented "compareVersions")
  , ("concatLists", nativeV $ notImplemented "concatLists")
  , ("currentSystem", nativeV $ notImplemented "currentSystem")
  , ("deepSeq", nativeV $ notImplemented "deepSeq")
  , ("div", nativeV $ notImplemented "div")
  , ("elem", nativeV $ notImplemented "elem")
  , ("elemAt", nativeV $ notImplemented "elemAt")
  , ("fetchurl", nativeV $ notImplemented "fetchurl")
  , ("filter", nativeV $ notImplemented "filter")
  , ("filterSource", nativeV $ notImplemented "filterSource")
  , ("fromJSON", nativeV $ notImplemented "fromJSON")
  , ("genList", nativeV $ notImplemented "genList")
  , ("getAttr", nativeV $ notImplemented "getAttr")
  , ("getEnv", nativeV $ notImplemented "getEnv")
  , ("hasAttr", nativeV $ notImplemented "hasAttr")
  , ("hashString", nativeV $ notImplemented "hashString")
  , ("head", nativeV $ notImplemented "head")
  , ("intersectAttrs", nativeV $ notImplemented "intersectAttrs")
  , ("isAttrs", nativeV builtin_isAttrs)
  , ("isBool", nativeV builtin_isBool)
  , ("isFunction", nativeV builtin_isFunction)
  , ("isInt", nativeV builtin_isInt)
  , ("isList", nativeV builtin_isList)
  , ("isString", nativeV builtin_isString)
  , ("length", nativeV builtin_length)
  , ("lessThan", nativeV $ notImplemented "lessThan")
  , ("listToAttrs", nativeV $ notImplemented "listToAttrs")
  , ("mul", nativeV $ notImplemented "mul")
  , ("parseDrvName", nativeV $ notImplemented "parseDrvName")
  , ("pathExists", nativeV $ notImplemented "pathExists")
  , ("readDir", nativeV $ notImplemented "readDir")
  , ("readFile", nativeV $ notImplemented "readFile")
  , ("replaceStrings", nativeV $ notImplemented "replaceStrings")
  , ("seq", nativeV builtin_seq)
  , ("sort", nativeV $ notImplemented "sort")
  , ("stringLength", nativeV $ notImplemented "stringLength")
  , ("sub", nativeV $ notImplemented "sub")
  , ("substring", nativeV $ notImplemented "substring")
  , ("tail", nativeV $ notImplemented "tail")
  , ("toFile", nativeV $ notImplemented "toFile")
  , ("toJSON", nativeV $ notImplemented "toJSON")
  , ("toPath", nativeV $ notImplemented "toPath")
  , ("toXML", nativeV $ notImplemented "toXML")
  , ("trace", nativeV $ notImplemented "trace")
  , ("typeOf", nativeV $ notImplemented "typeOf")
  ]

-- | The set of objects which should appear at top-level.
topLevelBuiltins :: AttrSet
topLevelBuiltins = mkEnv
  [ ("abort", nativeV $ notImplemented "abort")
  , ("builtins", VAttrSet builtins)
  , ("baseNameOf", nativeV $ notImplemented "baseNameOf")
  , ("derivation", nativeV $ notImplemented "derivation")
  , ("dirOf", nativeV $ notImplemented "dirOf")
  , ("fetchTarball", nativeV $ notImplemented "fetchTarball")
  , ("import", nativeV $ notImplemented "import")
  , ("isNull", nativeV builtin_isNull)
  , ("removeAttrs", nativeV $ notImplemented "removeAttrs")
  , ("throw", nativeV builtin_throw)
  , ("toString", nativeV builtin_toString)
  , ("seq", nativeV builtin_seq)
  , ("length", nativeV builtin_length)
  ]

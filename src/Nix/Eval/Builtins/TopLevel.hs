module Nix.Eval.Builtins.TopLevel where

import Nix.Common
import Nix.Types (NBinaryOp(..), NUnaryOp(..))
import Nix.Expressions
import Nix.Values.Generic
import Nix.Values.Lazy
import Nix.Values.NativeConversion
import Nix.Eval.Builtins.Operators
import Nix.Eval.Builtins.NativeFunctions
import Nix.Eval.Evaluator (evaluate)

-- | Throws a 'NotImplemented' error with the given name. We should be
-- able to get rid of this once the implementation is complete.
notImplemented :: Text -> LazyValue
notImplemented = throwError . NotImplemented

-- | The `builtins` object.
builtins :: LAttrSet
builtins = mkEnvL
  [ ("add", pure $ nativeV $ interpretBinop NPlus)
  , ("all", notImplemented "all")
  , ("any", notImplemented "any")
  , ("attrNames", pure $ nativeV $ toNative1 builtin_attrNames)
  , ("attrValues", pure $ nativeV $ toNative1 builtin_attrValues)
  , ("compareVersions", notImplemented "compareVersions")
  , ("concatLists", notImplemented "concatLists")
  , ("currentSystem", notImplemented "currentSystem")
  , ("deepSeq", pure $ nativeV $ toNative2L builtin_deepSeq)
  , ("div", pure $ nativeV $ interpretBinop NDiv)
  , ("elem", notImplemented "elem")
  , ("elemAt", pure $ nativeV $ toNative2 builtin_elemAt)
  , ("fetchurl", notImplemented "fetchurl")
  , ("filter", notImplemented "filter")
  , ("filterSource", notImplemented "filterSource")
  , ("fromJSON", notImplemented "fromJSON")
  , ("genList", notImplemented "genList")
  , ("getAttr", notImplemented "getAttr")
  , ("getEnv", notImplemented "getEnv")
  , ("hasAttr", notImplemented "hasAttr")
  , ("hashString", notImplemented "hashString")
  , ("head", pure $ nativeV $ toNative1 builtin_head)
  , ("intersectAttrs", pure $ nativeV $ toNative2 builtin_intersectAttrs)
  , ("isAttrs", pure $ nativeV $ toNative1 builtin_isAttrs)
  , ("isBool", pure $ nativeV $ toNative1 builtin_isBool)
  , ("isFunction", pure $ nativeV $ toNative1 builtin_isFunction)
  , ("isInt", pure $ nativeV $ toNative1 builtin_isInt)
  , ("isList", pure $ nativeV $ toNative1 builtin_isList)
  , ("isString", pure $ nativeV $ toNative1 builtin_isString)
  , ("length", pure $ nativeV $ toNative1 builtin_length)
  , ("lessThan", pure $ nativeV $ interpretBinop NLt)
  , ("listToAttrs", notImplemented "listToAttrs")
  , ("mul", pure $ nativeV $ interpretBinop NMult)
  , ("parseDrvName", notImplemented "parseDrvName")
  , ("pathExists", notImplemented "pathExists")
  , ("readDir", notImplemented "readDir")
  , ("readFile", notImplemented "readFile")
  , ("replaceStrings", notImplemented "replaceStrings")
  , ("seq", pure $ nativeV $ toNative2L builtin_seq)
  , ("sort", notImplemented "sort")
  , ("stringLength", pure $ nativeV $ toNative1 builtin_stringLength)
  , ("sub", pure $ nativeV $ interpretBinop NMinus)
  , ("substring", notImplemented "substring")
  , ("tail", pure $ nativeV $ toNative1 builtin_tail)
  , ("toFile", notImplemented "toFile")
  , ("toJSON", notImplemented "toJSON")
  , ("toPath", notImplemented "toPath")
  , ("toXML", notImplemented "toXML")
  , ("trace", notImplemented "trace")
  , ("typeOf", pure $ nativeV $ toNative1 builtin_typeOf)
  ]

-- | The set of objects which should appear at top-level.
topLevelBuiltins :: LAttrSet
topLevelBuiltins = mkEnvL
  [ ("abort", notImplemented "abort")
  , ("baseNameOf", notImplemented "baseNameOf")
  , ("builtins", pure $ VAttrSet builtins)
  , ("derivation", notImplemented "derivation")
  , ("dirOf", notImplemented "dirOf")
  , ("fetchTarball", notImplemented "fetchTarball")
  , ("import", notImplemented "import")
  , ("isNull", pure $ nativeV $ toNative1 builtin_isNull)
  , ("length", pure $ nativeV $ toNative1 builtin_length)
  , ("map", pure $ nativeV $ toNative2L' builtin_map)
  , ("removeAttrs", notImplemented "removeAttrs")
  , ("seq", pure $ nativeV $ toNative2L builtin_seq)
  , ("throw", pure $ nativeV $ toNative1 builtin_throw)
  , ("toString", pure $ nativeV $ toNative1 builtin_toString)
  ]

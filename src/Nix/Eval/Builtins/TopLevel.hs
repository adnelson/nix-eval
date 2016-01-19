module Nix.Eval.Builtins.TopLevel where

import Nix.Common
import Nix.Types (NBinaryOp(..), NUnaryOp(..))
import Nix.Expressions
import Nix.Values.Generic
import Nix.Values.Lazy
import Nix.Values.NativeConversion
import Nix.Eval.Errors (EvalError(NotImplemented))
import Nix.Eval.Builtins.Operators
import Nix.Eval.Builtins.NativeFunctions
import Nix.Eval.Evaluator (evaluate)

-- | Throws a 'NotImplemented' error with the given name. We should be
-- able to get rid of this once the implementation is complete.
notImplemented :: Monad m => Text -> LazyValue m
notImplemented = throwError . NotImplemented

-- | The `builtins` object.
builtins :: WriteMessage m => LAttrSet m
builtins = mkEnvL
  [ ("add", pNativeV $ interpretBinop NPlus)
  , ("all", notImplemented "all")
  , ("any", notImplemented "any")
  , ("attrNames", pNativeV $ toNative1 builtin_attrNames)
  , ("attrValues", pNativeV $ toNative1 builtin_attrValues)
  , ("compareVersions", notImplemented "compareVersions")
  , ("concatLists", notImplemented "concatLists")
  , ("currentSystem", notImplemented "currentSystem")
  , ("deepSeq", pNativeV $ toNative2L builtin_deepSeq)
  , ("div", pNativeV $ interpretBinop NDiv)
  , ("elem", pNativeV $ toNative2 builtin_elem)
  , ("elemAt", pNativeV $ toNative2 builtin_elemAt)
  , ("fetchurl", notImplemented "fetchurl")
  , ("filter", notImplemented "filter")
  , ("filterSource", notImplemented "filterSource")
  , ("fromJSON", notImplemented "fromJSON")
  , ("genList", notImplemented "genList")
  , ("getAttr", notImplemented "getAttr")
  , ("getEnv", notImplemented "getEnv")
  , ("hasAttr", pNativeV $ toNative2 builtin_hasAttr)
  , ("hashString", notImplemented "hashString")
  , ("head", pNativeV $ toNative1 builtin_head)
  , ("intersectAttrs", pNativeV $ toNative2 builtin_intersectAttrs)
  , ("isAttrs", pNativeV $ toNative1 builtin_isAttrs)
  , ("isBool", pNativeV $ toNative1 builtin_isBool)
  , ("isFunction", pNativeV $ toNative1 builtin_isFunction)
  , ("isInt", pNativeV $ toNative1 builtin_isInt)
  , ("isList", pNativeV $ toNative1 builtin_isList)
  , ("isString", pNativeV $ toNative1 builtin_isString)
  , ("length", pNativeV $ toNative1 builtin_length)
  , ("lessThan", pNativeV $ interpretBinop NLt)
  , ("listToAttrs", notImplemented "listToAttrs")
  , ("mul", pNativeV $ interpretBinop NMult)
  , ("parseDrvName", notImplemented "parseDrvName")
  , ("pathExists", notImplemented "pathExists")
  , ("readDir", notImplemented "readDir")
  , ("readFile", notImplemented "readFile")
  , ("replaceStrings", notImplemented "replaceStrings")
  , ("seq", pNativeV $ toNative2L builtin_seq)
  , ("sort", notImplemented "sort")
  , ("stringLength", pNativeV $ toNative1 builtin_stringLength)
  , ("sub", pNativeV $ interpretBinop NMinus)
  , ("substring", pNativeV $ toNative3 builtin_substring)
  , ("tail", pNativeV $ toNative1 builtin_tail)
  , ("toFile", notImplemented "toFile")
  , ("toJSON", notImplemented "toJSON")
  , ("toPath", notImplemented "toPath")
  , ("toXML", notImplemented "toXML")
  , ("trace", pNativeV $ toNative2L builtin_trace)
  , ("typeOf", pNativeV $ toNative1 builtin_typeOf)
  ]

-- | The set of objects which should appear at top-level.
topLevelBuiltins :: WriteMessage m => LAttrSet m
topLevelBuiltins = mkEnvL
  [ ("abort", notImplemented "abort")
  , ("baseNameOf", notImplemented "baseNameOf")
  , ("builtins", pure $ VAttrSet builtins)
  , ("derivation", notImplemented "derivation")
  , ("dirOf", notImplemented "dirOf")
  , ("fetchTarball", notImplemented "fetchTarball")
  , ("import", notImplemented "import")
  , ("isNull", pNativeV $ toNative1 builtin_isNull)
  , ("length", pNativeV $ toNative1 builtin_length)
  , ("map", pNativeV $ toNative2L' builtin_map)
  , ("removeAttrs", pNativeV $ toNative2 builtin_removeAttrs)
  , ("seq", pNativeV $ toNative2L builtin_seq)
  , ("throw", pNativeV $ toNative1 builtin_throw)
  , ("toString", pNativeV $ toNative1 builtin_toString)
  ]

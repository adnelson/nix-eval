module Nix.Eval.Values.Builtins.TopLevel where

import Nix.Common
import Nix.Eval.Values.Generic
import Nix.Eval.Values.Lazy
import Nix.Eval.Values.NativeConversion
import Nix.Eval.Values.Builtins.Operators
import Nix.Eval.Values.Builtins.NativeFunctions

-- | The `builtins` object.
builtins :: AttrSet
builtins = mkEnv
  [ ("add", nativeV $ interpretBinop NPlus)
  , ("all", nativeV $ notImplemented "all")
  , ("any", nativeV $ notImplemented "any")
  , ("attrNames", nativeV $ notImplemented "attrNames")
  , ("attrValues", nativeV $ notImplemented "attrValues")
  , ("compareVersions", nativeV $ notImplemented "compareVersions")
  , ("concatLists", nativeV $ notImplemented "concatLists")
  , ("currentSystem", nativeV $ notImplemented "currentSystem")
  , ("deepSeq", nativeV builtin_deepSeq)
  , ("div", nativeV $ interpretBinop NDiv)
  , ("elem", nativeV $ notImplemented "elem")
  , ("elemAt", nativeV builtin_elemAt)
  , ("fetchurl", nativeV $ notImplemented "fetchurl")
  , ("filter", nativeV $ notImplemented "filter")
  , ("filterSource", nativeV $ notImplemented "filterSource")
  , ("fromJSON", nativeV $ notImplemented "fromJSON")
  , ("genList", nativeV $ notImplemented "genList")
  , ("getAttr", nativeV $ notImplemented "getAttr")
  , ("getEnv", nativeV $ notImplemented "getEnv")
  , ("hasAttr", nativeV $ notImplemented "hasAttr")
  , ("hashString", nativeV $ notImplemented "hashString")
  , ("head", nativeV builtin_head)
  , ("intersectAttrs", nativeV $ notImplemented "intersectAttrs")
  , ("isAttrs", nativeV builtin_isAttrs)
  , ("isBool", nativeV builtin_isBool)
  , ("isFunction", nativeV builtin_isFunction)
  , ("isInt", nativeV builtin_isInt)
  , ("isList", nativeV builtin_isList)
  , ("isString", nativeV builtin_isString)
  , ("length", nativeV builtin_length)
  , ("lessThan", nativeV $ interpretBinop NLt)
  , ("listToAttrs", nativeV $ notImplemented "listToAttrs")
  , ("mul", nativeV $ interpretBinop NMult)
  , ("parseDrvName", nativeV $ notImplemented "parseDrvName")
  , ("pathExists", nativeV $ notImplemented "pathExists")
  , ("readDir", nativeV $ notImplemented "readDir")
  , ("readFile", nativeV $ notImplemented "readFile")
  , ("replaceStrings", nativeV $ notImplemented "replaceStrings")
  , ("seq", nativeV builtin_seq)
  , ("sort", nativeV $ notImplemented "sort")
  , ("stringLength", nativeV $ notImplemented "stringLength")
  , ("sub", nativeV $ interpretBinop NMinus)
  , ("substring", nativeV $ notImplemented "substring")
  , ("tail", nativeV $ notImplemented "tail")
  , ("toFile", nativeV $ notImplemented "toFile")
  , ("toJSON", nativeV $ notImplemented "toJSON")
  , ("toPath", nativeV $ notImplemented "toPath")
  , ("toXML", nativeV $ notImplemented "toXML")
  , ("trace", nativeV $ notImplemented "trace")
  , ("typeOf", nativeV builtin_typeOf)
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

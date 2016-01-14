module Nix.Eval.Values.Builtins.TopLevel where

import Nix.Common
import Nix.Types (NBinaryOp(..), NUnaryOp(..))
import Nix.Eval.Expressions
import Nix.Eval.Values.Generic
import Nix.Eval.Values.Lazy
import Nix.Eval.Values.NativeConversion
import Nix.Eval.Values.Builtins.Operators
import Nix.Eval.Values.Builtins.NativeFunctions
import Nix.Eval.Evaluator (evaluate, evalApply)

-- | Evaluate an expression with the builtins in scope.
performEval :: Expression -> LazyValue
performEval = evaluate topLevelBuiltins

-- | Maps a function over a list.
builtin_map :: LazyValue -> WHNFValue -> LazyValue
builtin_map func = \case
  VList list -> pure $ VList $ map (evalApply func) list
  val -> expectedList val

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
  , ("attrNames", notImplemented "attrNames")
  , ("attrValues", notImplemented "attrValues")
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
  , ("intersectAttrs", notImplemented "intersectAttrs")
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
  , ("stringLength", notImplemented "stringLength")
  , ("sub", pure $ nativeV $ interpretBinop NMinus)
  , ("substring", notImplemented "substring")
  , ("tail", notImplemented "tail")
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

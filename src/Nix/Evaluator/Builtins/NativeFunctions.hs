module Nix.Evaluator.Builtins.NativeFunctions where

import Nix.Common
import Nix.Atoms
import Nix.Expr (NBinaryOp(..), NUnaryOp(..))
import Nix.Evaluator.Contexts (WriteMessage(..))
import Nix.Evaluator.Evaluator (evalApply)
import Nix.Evaluator.Errors
import Nix.Evaluator.RuntimeTypes
import Nix.Values
import Nix.Values.NativeConversion
import Nix.Evaluator.Builtins.Operators (binop_eq)

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H

-- | Convert a value to a env-variable-compatible string.
valueToEnvString :: Monad ctx => WHNFValue ctx -> Eval ctx Text
valueToEnvString val = case val of
  VConstant c -> pure $ atomToEnvString c
  VString s -> pure s
  VList vals -> do
    strings <- forM vals $ \lazyVal -> do
      val <- lazyVal
      valueToEnvString val
    pure $ intercalate " " $ toList strings
  _ -> do
    let types = [RT_String, RT_Path, RT_Bool, RT_Int, RT_Null, RT_List]
    expectedOneOf types val

-- | Convert a value to a string.
builtin_toString :: Monad m => WHNFValue m -> LazyValue m
builtin_toString val = map strV $ valueToEnvString val

-- | Evaluate the first argument to weak-head normal form, and as long
-- as this first evaluation succeeds, return the second argument. If
-- the first evaluation fails, this function fails. Note that the
-- function body here doesn't actually /do/ anything. Simply by the
-- fact that the first argument is in WHNF, it means that the function
-- will fail if the first argument is an error.
builtin_seq :: Monad m => WHNFValue m -> LazyValue m -> LazyValue m
builtin_seq = \_ -> id

-- | The throw function forces an error to occur.
builtin_throw :: Monad m => WHNFValue m -> LazyValue m
builtin_throw val = case val of
  VString msg -> throwError $ CustomError msg
  _ -> expectedString val

-- | Asserts its first argument is true, and then returns its second.
builtin_assert :: Monad m => WHNFValue m -> LazyValue m -> LazyValue m
builtin_assert val res = case val of
  VConstant (NBool True) -> res
  VConstant (NBool False) -> throwError AssertionError
  _ -> expectedBool val

-- | Get the length of a list.
builtin_length :: Monad m => WHNFValue m -> LazyValue m
builtin_length val = case val of
  VList vals -> pure $ convert (length vals)
  v -> expectedList v

-- | Add to the front of a list.
builtin_cons :: Monad m => LazyValue m -> WHNFValue m -> LazyValue m
builtin_cons val v = case v of
  VList list -> pure $ VList $ (val `cons` list)
  _ -> expectedList v

-- | Index into list. The list is the first argument.
builtin_elemAt :: Monad m => WHNFValue m -> WHNFValue m -> LazyValue m
builtin_elemAt val1 val2 = case val1 of
  VList list -> case val2 of
    VConstant (NInt i) | i < 0 -> throwError $ IndexError i (length list)
    VConstant (NInt i) -> case list `index` fromIntegral i of
      Nothing -> throwError $ IndexError i (length list)
      Just val -> val
    _ -> expectedInt val2
  _ -> expectedList val1

-- | Get the head of a list.
builtin_head :: Monad m => WHNFValue m -> LazyValue m
builtin_head val = case val of
  VList list -> case uncons list of
    Nothing -> throwError EmptyList
    Just (first, _) -> first
  _ -> expectedList val

-- | Get the tail of a list.
builtin_tail :: Monad m => WHNFValue m -> LazyValue m
builtin_tail val = case val of
  VList list -> case uncons list of
    Nothing -> throwError EmptyList
    Just (_, rest) -> pure $ VList rest
  _ -> expectedList val

-- | Maps a function over a list.
builtin_map :: Monad m => LazyValue m -> WHNFValue m -> LazyValue m
builtin_map func = \case
  VList list -> pure $ VList $ map (evalApply func) list
  val -> expectedList val

-- | Creates an `isX` function given a type to test a value against.
mkTypeTest :: Monad m => RuntimeType -> WHNFValue m -> LazyValue m
mkTypeTest type_ = map convert . hasType type_

-- | A bunch of runtime type checking tests.
builtin_isAttrs, builtin_isList, builtin_isFunction, builtin_isInt,
  builtin_isBool, builtin_isNull, builtin_isString, builtin_isPath
  :: Monad m => WHNFValue m -> LazyValue m
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
builtin_deepSeq :: Monad m => WHNFValue m -> LazyValue m -> LazyValue m
builtin_deepSeq val x = deeplyEval val >> x

-- | Get the type of a value as a string.
builtin_typeOf :: Monad m => WHNFValue m -> LazyValue m
builtin_typeOf v = VString . typeToString <$> typeOf v

-- | Get the length of a nix string.
builtin_stringLength :: Monad m => WHNFValue m -> LazyValue m
builtin_stringLength = \case
  VString s -> convert (length s)
  v -> expectedString v

-- | Get all of the keys from a set as a list of strings.
builtin_attrNames :: Monad m => WHNFValue m -> LazyValue m
builtin_attrNames = \case
  VAttrSet attrs -> pure $ VList $ map (pure . strV) $ envKeyList attrs
  v -> expectedAttrs v

-- | Get all of the values from a set as a list of strings.
builtin_attrValues :: Monad m => WHNFValue m -> LazyValue m
builtin_attrValues = \case
  VAttrSet attrs -> pure $ VList $ envValueList attrs
  v -> expectedAttrs v

-- | Return a set consisting of the attributes in the set e2 that also
-- exist in the set e1. If keys are shared, the values from the second
-- set will appear.
builtin_intersectAttrs :: Monad m => WHNFValue m -> WHNFValue m -> LazyValue m
builtin_intersectAttrs = \case
  VAttrSet (Environment set1) -> \case
    VAttrSet (Environment set2) -> do
      -- We flip the order of set1 and set2 here because H.intersection
      -- will prefer the values of the first set, where we want the second.
      pure $ VAttrSet $ Environment $ H.intersection set2 set1
    v -> expectedAttrs v
  v -> \_ -> expectedAttrs v

-- | First argument is the name of an attribute, and the second is a
-- set. Returns whether that attribute is in the set.
builtin_hasAttr :: Monad m => WHNFValue m -> WHNFValue m -> LazyValue m
builtin_hasAttr attrName attrSet = case (attrName, attrSet) of
  (VString name, VAttrSet aset) -> case lookupEnv name aset of
    Nothing -> convert False
    Just _ -> convert True
  (VString _, _) -> expectedString attrName
  _ -> expectedAttrs attrSet

-- | First argument is an attribute set, second is a list of strings.
-- Remove all keys in the list from the set.
builtin_removeAttrs :: forall m. Monad m => WHNFValue m -> WHNFValue m -> LazyValue m
builtin_removeAttrs attrSet attrList = case (attrSet, attrList) of
  (VAttrSet set, VList names) -> go set $ toList names where
     go res [] = pure $ VAttrSet res
     go res (lval:lvals) = lval >>= \case
       VString name -> go (deleteEnv name res) lvals
       v -> expectedString v
  (VAttrSet _, _) -> expectedList attrList
  _ -> expectedAttrs attrSet

-- | `start` and `len` are ints, `str` is a string.
-- Take `len` characters from `str` starting at character `start`.
builtin_substring :: forall m. Monad m =>
                     WHNFValue m -> WHNFValue m -> WHNFValue m -> LazyValue m
builtin_substring start len str = case (start, len, str) of
  (VConstant (NInt start), VConstant (NInt len), VString str) -> do
    pure $ strV $ substring start len str
  (VConstant (NInt _), VConstant (NInt _), v) -> expectedString v
  (VConstant (NInt _), v, _) -> expectedInt v
  (v, _, _) -> expectedInt v

-- | See if the first argument is a member of the second (a list).
builtin_elem :: forall m. Monad m => WHNFValue m -> WHNFValue m -> LazyValue m
builtin_elem item = \case
  VList list -> go $ toList list where
    go [] = convert False
    go (lval:lvals) = do
      val <- lval :: LazyValue m
      binop_eq item val >>= \case
        VConstant (NBool True) -> convert True
        _ -> go lvals
  v -> expectedList v

-- | Output the first argument as a string, and return the second
-- argument.
builtin_trace :: forall m. WriteMessage m =>
                 WHNFValue m -> LazyValue m -> LazyValue m
builtin_trace val lval = do
  message <- valueToEnvString val
  writeMessage message
  lval

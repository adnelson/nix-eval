module Nix.Spec.BuiltinsSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Nix.Common
import Nix.Atoms (NAtom(..))
import Nix.Expr
import Nix.Eval
import Nix.Values
import Nix.Values.NativeConversion
import Nix.Evaluator
import Nix.Spec.Lib
import Nix.Spec.ExpectedBuiltins
import qualified Data.Set as S
import qualified Data.Text as T

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  valueToEnvStringSpec
  mkTypeTestSpec
  deepSeqSpec
  omapM_ describeTopLevel topLevelKeys
  omapM_ describeBuiltinKey keysInBuiltins

bi :: Text -> Expression
bi key = "builtins" !. key

valueToEnvStringSpec :: Spec
valueToEnvStringSpec = describe "valueToEnvString" $ do
  it "should translate lists" $ property $ \strs -> do
    let expected = intercalate " " $ toList strs
    res <- run $ valueToEnvString (VList $ map (pure . VString) strs)
    res `shouldBe` Right expected
  it "should translate nested lists" $ do
    res <- run $ valueToEnvString (listV ["hey", listV ["yo", "hi"]])
    res `shouldBe` Right "hey yo hi"
  it "should not translate sets" $ do
    res <- run $ valueToEnvString (attrsV [("hey", strV "hi")])
    res `shouldSatisfy` \case
      Left _ -> True
      Right _ -> False

mkTypeTestSpec :: Spec
mkTypeTestSpec = describe "mkTypeTest function" $ do
  it "should test types of constants correctly" $ do
    property $ \constant -> do
      let test = mkTypeTest (typeOfAtom constant)
      res <- runStrictL1 $ test (fromAtom constant)
      res `shouldBe` Right (convert True)
  it "should test lists correctly" $ do
    property $ \constantList -> do
      res <- runStrictL1 $ mkTypeTest RT_List $ fromAtoms constantList
      res `shouldBe` Right (convert True)
  it "should test sets correctly" $ do
    property $ \constantSet -> do
      res <- runStrictL1 $ mkTypeTest RT_Set $ fromAtomSet constantSet
      res `shouldBe` Right (convert True)

deepSeqSpec :: Spec
deepSeqSpec = describe "deepSeq" $ do
  it "should error on evaluating something that contains an error" $ do
    shouldErrorWith (bi "deepSeq" @@ attrsE [("x", failingExpression)]
                                  @@ succeedingExpression)
                     ["CustomError", "failed on purpose"]
  it "should return the second argument if no error" $ do
    shouldEval (bi "deepSeq" @@ attrsE [("x", 1)]
                             @@ succeedingExpression)

-- | Defines a bunch of tests for top-level builtin functions.
describeTopLevel :: Text -> Spec
describeTopLevel name = case name of
  "map" -> wrapDescribe mapSpec
  "isNull" -> wrapDescribe $ do
    it "should be true for null" $ do
      "isNull" @@ mkNull `shouldEvalTo` convert True
    it "should be false for others" $ do
      "isNull" @@ mkList [] `shouldEvalTo` convert False
      "isNull" @@ mkInt 1 `shouldEvalTo` convert False
  "removeAttrs" -> wrapDescribe $ do
    it "should remove keys" $ do
      let aset = attrsE [("foo", 1), ("bar", 2), ("baz", 3)]
          toRm = mkList [mkStr "foo", mkStr "bar"]
      "removeAttrs" @@ aset @@ toRm `shouldEvalTo` attrsV [("baz", intV 3)]
    it "shouldn't complain if a key isn't present" $ do
      "removeAttrs" @@ attrsE [] @@ mkList [mkStr "x"] `shouldEvalTo` attrsV []

  name -> it "isn't written yet" $ do
    pendingWith $ "No tests yet defined for " <> show name
  where
    wrapDescribe = describe ("top-level builtin " <> show name)
    mapSpec = do
      it "should map a function over a list" $ property $ \list ->
        "map" @@ ("x" --> "x") @@ fromAtoms list
          `shouldEvalTo` fromAtoms list
      it "should map the +1 function over a list" $ property $ \nums -> do
          "map" @@ ("x" --> "x" + 1) @@ (fromAtoms $ map NInt nums)
            `shouldEvalTo` fromAtoms (map (\i -> NInt (i + 1)) nums)
      it "should map over a nested list" $ property $ \nums1 nums2 -> do
        let list1 = fromAtoms $ map NInt nums1
            list2 = fromAtoms $ map NInt nums2
            list3 = mkList [list1, list2]
        -- The ID function over a nested list
        "map" @@ ("x" --> "x") @@ list3
          `shouldEvalTo` listV (map (fromAtoms . map NInt) [nums1, nums2])
        -- Mapping the map function
        "map" @@ ("map" @@ ("x" --> "x")) @@ list3
          `shouldEvalTo` listV [fromAtoms $ map NInt nums1,
                                fromAtoms $ map NInt nums2]
        -- Mapping functions which do things
        "map" @@ ("map" @@ ("x" --> "x" * 2)) @@ list3
          `shouldEvalTo` listV [fromAtoms $ map (NInt . (*2)) nums1,
                                fromAtoms $ map (NInt . (*2)) nums2]

-- | Defines a bunch of tests for keys appearing in the builtins
-- object.
describeBuiltinKey :: Text -> Spec
describeBuiltinKey name = case name of
  "length" -> wrapDescribe lengthSpec
  "add" -> wrapSingle $ property $ \i j ->
    bi "add" @@ mkInt i @@ mkInt j `shouldEvalTo` intV (i + j)
  "sub" -> wrapSingle $ property $ \i j ->
    bi "sub" @@ mkInt i @@ mkInt j `shouldEvalTo` intV (i - j)
  "mul" -> wrapSingle $ property $ \i j ->
    bi "mul" @@ mkInt i @@ mkInt j `shouldEvalTo` intV (i * j)
  "div" -> wrapSingle $ property $ \i j -> do
    case j of
      0 -> bi "div" @@ mkInt i @@ mkInt j `shouldErrorWith` ["DivideByZero"]
      _ -> bi "div" @@ mkInt i @@ mkInt j `shouldEvalTo` intV (i `div` j)
  "lessThan" -> wrapSingle $ property $ \i j -> do
    bi "lessThan" @@ mkInt i @@ mkInt j `shouldEvalTo` convert (i < j)
  "seq" -> wrapDescribe $ do
    it "should fail if first argument is error" $ do
      property $ \expr ->
        shouldError $ bi "seq" @@ failingExpression @@ expr
    it "should return second argument if first argument succeeds" $ do
      property $ \constant -> do
        let expr = fromAtom constant
        res <- evalStrict1 expr
        seqRes <- evalStrict1 (bi "seq" @@ succeedingExpression @@ expr)
        res `shouldBe` seqRes
  "deepSeq" -> deepSeqSpec
  "head" -> wrapDescribe $ do
    it "should get the head of a non-empty list" $ do
      let list = mkList [convertI 1]
      res <- evalStrict1 $ bi "head" @@ list
      res `shouldBe` Right (convert (1 :: Integer))
    it "should error on an empty list" $ do
      bi "head" @@ (mkList []) `shouldErrorWith` ["EmptyList"]
  "tail" -> wrapDescribe $ do
    it "should get the tail of a non-empty list" $ do
      res <- evalStrict1 $ bi "tail" @@ mkList [convertI 1, convertI 2]
      res `shouldBe` Right (fromAtoms [NInt 2])
    it "should error on an empty list" $ do
      bi "tail" @@ (mkList []) `shouldErrorWith` ["EmptyList"]
  "elemAt" -> wrapDescribe $ do
    it "should index correctly" $ do
      elem <- evalStrict1 $ bi "elemAt" @@ mkList [mkInt 1, mkInt 2] @@ convertI 1
      elem `shouldBe` Right (convert (2 :: Integer))
    it "should error on an empty list" $ do
      property $ \(i::Int) -> do
        let elem = builtin_elemAt (listV []) (convert i)
        elem `shouldBeErrorWith` ["IndexError"]
  "isAttrs" -> wrapDescribe $ do
    it "should be true for attribute sets" $ do
      bi "isAttrs" @@ attrsE [] `shouldEvalTo` convert True
    it "should be false for others" $ do
      bi "isAttrs" @@ mkList [] `shouldEvalTo` convert False
      bi "isAttrs" @@ mkInt 1 `shouldEvalTo` convert False
  "isList" -> wrapDescribe $ do
    it "should be true for lists" $ do
      bi "isList" @@ mkList [] `shouldEvalTo` convert True
    it "should be false for others" $ do
      bi "isList" @@ attrsE [] `shouldEvalTo` convert False
      bi "isList" @@ mkInt 1 `shouldEvalTo` convert False
  "isInt" -> wrapDescribe $ do
    it "should be true for integers" $ do
      bi "isInt" @@ mkInt 1 `shouldEvalTo` convert True
    it "should be false for others" $ do
      bi "isInt" @@ mkList [] `shouldEvalTo` convert False
      bi "isInt" @@ attrsE [] `shouldEvalTo` convert False
  "isBool" -> wrapDescribe $ do
    it "should be true for booleans" $ do
      bi "isBool" @@ mkBool False `shouldEvalTo` convert True
      bi "isBool" @@ mkBool True `shouldEvalTo` convert True
    it "should be false for others" $ do
      bi "isBool" @@ mkList [] `shouldEvalTo` convert False
      bi "isBool" @@ attrsE [] `shouldEvalTo` convert False
  "isString" -> wrapDescribe $ do
    it "should be true for strings" $ do
      bi "isString" @@ mkStr "hello" `shouldEvalTo` convert True
    it "should be false for others" $ do
      bi "isString" @@ mkList [] `shouldEvalTo` convert False
      bi "isString" @@ mkInt 1 `shouldEvalTo` convert False
  "isFunction" -> wrapDescribe $ do
    it "should be true for functions" $ do
      bi "isFunction" @@ ("x" --> "x") `shouldEvalTo` convert True
      bi "isFunction" @@ bi "isFunction" `shouldEvalTo` convert True
      -- TODO test functions that unpack attribute set args
    it "should be false for others" $ do
      bi "isFunction" @@ mkList [] `shouldEvalTo` convert False
      bi "isFunction" @@ mkInt 1 `shouldEvalTo` convert False
  "typeOf" -> wrapDescribe $ do
    let mkTest name e = it ("should work for " <> unpack name) $ do
          bi "typeOf" @@ e `shouldEvalTo` strV name
    mkTest "null" mkNull
    mkTest "list" (mkList [])
    mkTest "set" (attrsE [])
    mkTest "lambda" ("x" --> "x")
    mkTest "lambda" (bi "typeOf")
    mkTest "int" 1
    mkTest "bool" (mkBool False)
  "stringLength" -> wrapDescribe $ do
    it "should get the length" $ property $ \s ->
      bi "stringLength" @@ mkStr s `shouldEvalTo` convert (length s)
  "attrNames" -> wrapDescribe $ do
    it "should get the names of the set" $ do
      property $ \keys -> do
        let uniqueKeys = S.toList $ S.fromList keys
        -- Make an attribute set from our random list of keys. The
        -- values are irrelevant so just make them null.
        let aset = attrsE $ zip uniqueKeys (repeat mkNull)
        -- This test is not quite as clean as it should be, due to the
        -- problem of equality testing requiring ordering.
        Right (VList names) <- evalStrict1 $ bi "attrNames" @@ aset
        sort names `shouldBe` map (pure . strV) (fromList $ sort uniqueKeys)
  "attrValues" -> wrapDescribe $ do
    it "should get the values of the set" $ do
      property $ \constants -> do
        let aset = attrsE $ zip (map tshow [1..]) (map fromAtom constants)
        -- Similarly kind of gross due to order-based equality.
        Right (VList values) <- evalStrict1 $ bi "attrValues" @@ aset
        let eval'dAtoms = map (\(Identity (VConstant c)) -> c) values
        sort eval'dAtoms `shouldBe` fromList (sort constants)
  "intersectAttrs" -> wrapDescribe $ do
    it "a set intersected with itself should equal itself" $ do
      property $ \constants -> do
        let aset = attrsE $ zip (map tshow [1..]) (map fromAtom constants)
        asetEval'd <- evalStrict1 aset
        intersectedEval'd <- evalStrict1 $ bi "intersectAttrs" @@ aset @@ aset
        asetEval'd `shouldBe` intersectedEval'd
    it "a set intersected with empty should be empty" $ do
      property $ \constants -> do
        let aset = attrsE $ zip (map tshow [1..]) (map fromAtom constants)
        intersectedEval'd <- evalStrict1 $
           bi "intersectAttrs" @@ aset @@ attrsE []
        intersectedEval'd `shouldBe` pure (attrsV [])
        -- Reverse the order.
        intersectedEval'd <- evalStrict1 $
           bi "intersectAttrs" @@ attrsE [] @@ aset
        intersectedEval'd `shouldBe` pure (attrsV [])
    it "should favor the second dictionary" $ do
      let (set1, set2) = (attrsE [("foo", 1)], attrsE [("foo", 2)])
      bi "intersectAttrs" @@ set1 @@ set2
        `shouldEvalTo` attrsV [("foo", intV 2)]
  "hasAttr" -> wrapDescribe $ do
    it "should work" $ do
      property $ \key -> do
        bi "hasAttr" @@ mkStr key @@ attrsE [(key, 1)] `shouldEvalTo` convert True
        bi "hasAttr" @@ mkStr key @@ attrsE [] `shouldEvalTo` convert False
  "substring" -> wrapDescribe $ do
    it "should take a substring" $ property $ \start len str -> do
      bi "substring" @@ mkInt start @@ mkInt len @@ mkStr str
        `shouldEvalTo` strV (substring start len str)
  "elem" -> wrapDescribe $ do
    it "should find it if it's there" $ do
      -- TODO: randomize the list?
      property $ \const1 const2 const3 -> do
        let list = fromAtoms [const1, const2, const3]
        bi "elem" @@ fromAtom const1 @@ list `shouldEvalTo` convert True
        bi "elem" @@ fromAtom const2 @@ list `shouldEvalTo` convert True
        bi "elem" @@ fromAtom const3 @@ list `shouldEvalTo` convert True
    it "shouldn't find it if it isn't there" $ do
      property $ \constant -> do
        let list = mkList []
        bi "elem" @@ fromAtom constant @@ list `shouldEvalTo` convert False
  "trace" -> wrapDescribe $ do
    it "should write a message" $ do
      let expr = bi "trace" @@ mkStr "hello!" @@ 1
      (result, state) <- evalStrict expr
      result `shouldBe` pure (intV 1)
      msWriteBuffer state `shouldBe` fromList ["hello!"]
    it "shouldn't write a message if the expression doesn't get evaluated" $ do
      let traced = bi "trace" @@ mkStr "hello!" @@ 1
          -- Here we're never using the second argument, so the
          -- expression shouldn't ever get evaluated.
          expr = ("x" --> ("_" --> "x")) @@ 3 @@ traced
      (result, state) <- evalStrict expr
      result `shouldBe` pure (intV 3)
      msWriteBuffer state `shouldBe` fromList []

-- For others, we just say the test is pending.
  name -> it "isn't written yet" $ do
    pendingWith $ "No tests yet defined for " <> show name
  where
    wrapDescribe = describe ("builtin key " <> show name)
    wrapSingle test = wrapDescribe $ it "should work" $ test
    lengthSpec = do
      it "should get the length of a list" $ property $ \list ->
        bi "length" @@ fromAtoms list
          `shouldEvalTo` convert (length list)
      it "shouldn't matter if the list has an error value" $ do
        property $ \list -> do
          bi "length" @@ (mkList (failingExpression : map fromAtom list))
            `shouldEvalTo` convert (length list + 1)

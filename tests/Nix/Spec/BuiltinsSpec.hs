module Nix.Spec.BuiltinsSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Nix.Common
import Nix.Expressions
import Nix.Eval
import Nix.Constants
import Nix.Values
import Nix.Values.NativeConversion
import Nix.Spec.Lib
import Nix.Spec.ExpectedBuiltins
import qualified Data.Set as S
import qualified Data.Text as T

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  constantToEnvStringSpec
  valueToEnvStringSpec
  mkTypeTestSpec
  deepSeqSpec
  omapM_ describeTopLevel topLevelKeys
  omapM_ describeBuiltinKey keysInBuiltins

bi :: Text -> Expression
bi key = "builtins" !. key

constantToEnvStringSpec :: Spec
constantToEnvStringSpec = describe "constantToEnvString" $ do
  it "should translate strings" $ property $ \s ->
    constantToEnvString (String s) `shouldBe` s
  it "should translate numbers" $ property $ \i ->
    constantToEnvString (Int i) `shouldBe` tshow i

valueToEnvStringSpec :: Spec
valueToEnvStringSpec = describe "valueToEnvString" $ do
  it "should translate lists" $ property $ \strs -> do
    let strCs = map String strs
        expected = intercalate " " strs
    res <- run $ valueToEnvString (fromConstants strCs)
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
      let test = mkTypeTest (typeOfConstant constant)
      res <- runStrictL1 $ test (fromConstant constant)
      res `shouldBe` Right (convert True)
  it "should test lists correctly" $ do
    property $ \constantList -> do
      res <- runStrictL1 $ mkTypeTest RT_List $ fromConstants constantList
      res `shouldBe` Right (convert True)
  it "should test sets correctly" $ do
    property $ \constantSet -> do
      res <- runStrictL1 $ mkTypeTest RT_Set $ fromConstantSet constantSet
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
      "isNull" @@ nullE `shouldEvalTo` convert True
    it "should be false for others" $ do
      "isNull" @@ listE [] `shouldEvalTo` convert False
      "isNull" @@ intE 1 `shouldEvalTo` convert False
  "removeAttrs" -> wrapDescribe $ do
    it "should remove keys" $ do
      let aset = attrsE [("foo", 1), ("bar", 2), ("baz", 3)]
          toRm = listE [strE "foo", strE "bar"]
      "removeAttrs" @@ aset @@ toRm `shouldEvalTo` attrsV [("baz", intV 3)]
    it "shouldn't complain if a key isn't present" $ do
      "removeAttrs" @@ attrsE [] @@ listE [strE "x"] `shouldEvalTo` attrsV []

  name -> it "isn't written yet" $ do
    pendingWith $ "No tests yet defined for " <> show name
  where
    wrapDescribe = describe ("top-level builtin " <> show name)
    mapSpec = do
      it "should map a function over a list" $ property $ \list ->
        "map" @@ ("x" --> "x") @@ fromConstants list
          `shouldEvalTo` fromConstants list
      it "should map the +1 function over a list" $ property $ \nums -> do
          "map" @@ ("x" --> "x" + 1) @@ (fromConstants $ map Int nums)
            `shouldEvalTo` fromConstants (map (\i -> Int (i + 1)) nums)
      it "should map over a nested list" $ property $ \nums1 nums2 -> do
        let list1 = fromConstants $ map Int nums1
            list2 = fromConstants $ map Int nums2
            list3 = listE [list1, list2]
        -- The ID function over a nested list
        "map" @@ ("x" --> "x") @@ list3
          `shouldEvalTo` listV (map (fromConstants . map Int) [nums1, nums2])
        -- Mapping the map function
        "map" @@ ("map" @@ ("x" --> "x")) @@ list3
          `shouldEvalTo` listV [fromConstants $ map Int nums1,
                                fromConstants $ map Int nums2]
        -- Mapping functions which do things
        "map" @@ ("map" @@ ("x" --> "x" * 2)) @@ list3
          `shouldEvalTo` listV [fromConstants $ map (Int . (*2)) nums1,
                                fromConstants $ map (Int . (*2)) nums2]

-- | Defines a bunch of tests for keys appearing in the builtins
-- object.
describeBuiltinKey :: Text -> Spec
describeBuiltinKey name = case name of
  "length" -> wrapDescribe lengthSpec
  "add" -> wrapSingle $ property $ \i j ->
    bi "add" @@ intE i @@ intE j `shouldEvalTo` intV (i + j)
  "sub" -> wrapSingle $ property $ \i j ->
    bi "sub" @@ intE i @@ intE j `shouldEvalTo` intV (i - j)
  "mul" -> wrapSingle $ property $ \i j ->
    bi "mul" @@ intE i @@ intE j `shouldEvalTo` intV (i * j)
  "div" -> wrapSingle $ property $ \i j -> do
    case j of
      0 -> bi "div" @@ intE i @@ intE j `shouldErrorWith` ["DivideByZero"]
      _ -> bi "div" @@ intE i @@ intE j `shouldEvalTo` intV (i `div` j)
  "lessThan" -> wrapSingle $ property $ \i j -> do
    bi "lessThan" @@ intE i @@ intE j `shouldEvalTo` convert (i < j)
  "seq" -> wrapDescribe $ do
    it "should fail if first argument is error" $ do
      property $ \expr ->
        shouldError $ bi "seq" @@ failingExpression @@ expr
    it "should return second argument if first argument succeeds" $ do
      property $ \constant -> do
        let expr = fromConstant constant
        res <- evalStrict1 expr
        seqRes <- evalStrict1 (bi "seq" @@ succeedingExpression @@ expr)
        res `shouldBe` seqRes
  "deepSeq" -> deepSeqSpec
  "head" -> wrapDescribe $ do
    it "should get the head of a non-empty list" $ do
      let list = listE [fromInt 1]
      res <- evalStrict1 $ bi "head" @@ list
      res `shouldBe` Right (convert (1 :: Integer))
    it "should error on an empty list" $ do
      bi "head" @@ (listE []) `shouldErrorWith` ["EmptyList"]
  "tail" -> wrapDescribe $ do
    it "should get the tail of a non-empty list" $ do
      let list = listE [fromInt 1, fromInt 2]
      res <- evalStrict1 $ bi "tail" @@ list
      res `shouldBe` Right (fromConstants [Int 2])
    it "should error on an empty list" $ do
      bi "tail" @@ (listE []) `shouldErrorWith` ["EmptyList"]
  "elemAt" -> wrapDescribe $ do
    it "should index correctly" $ do
      elem <- evalStrict1 $ bi "elemAt" @@ listE [intE 1, intE 2] @@ fromInt 1
      elem `shouldBe` Right (convert (2 :: Integer))
    it "should error on an empty list" $ do
      property $ \(i::Int) -> do
        let elem = builtin_elemAt (listV []) (fromInt i)
        elem `shouldBeErrorWith` ["IndexError"]
  "isAttrs" -> wrapDescribe $ do
    it "should be true for attribute sets" $ do
      bi "isAttrs" @@ attrsE [] `shouldEvalTo` convert True
    it "should be false for others" $ do
      bi "isAttrs" @@ listE [] `shouldEvalTo` convert False
      bi "isAttrs" @@ intE 1 `shouldEvalTo` convert False
  "isList" -> wrapDescribe $ do
    it "should be true for lists" $ do
      bi "isList" @@ listE [] `shouldEvalTo` convert True
    it "should be false for others" $ do
      bi "isList" @@ attrsE [] `shouldEvalTo` convert False
      bi "isList" @@ intE 1 `shouldEvalTo` convert False
  "isInt" -> wrapDescribe $ do
    it "should be true for integers" $ do
      bi "isInt" @@ intE 1 `shouldEvalTo` convert True
    it "should be false for others" $ do
      bi "isInt" @@ listE [] `shouldEvalTo` convert False
      bi "isInt" @@ attrsE [] `shouldEvalTo` convert False
  "isBool" -> wrapDescribe $ do
    it "should be true for booleans" $ do
      bi "isBool" @@ boolE False `shouldEvalTo` convert True
      bi "isBool" @@ boolE True `shouldEvalTo` convert True
    it "should be false for others" $ do
      bi "isBool" @@ listE [] `shouldEvalTo` convert False
      bi "isBool" @@ attrsE [] `shouldEvalTo` convert False
  "isString" -> wrapDescribe $ do
    it "should be true for strings" $ do
      bi "isString" @@ strE "hello" `shouldEvalTo` convert True
    it "should be false for others" $ do
      bi "isString" @@ listE [] `shouldEvalTo` convert False
      bi "isString" @@ intE 1 `shouldEvalTo` convert False
  "isFunction" -> wrapDescribe $ do
    it "should be true for functions" $ do
      bi "isFunction" @@ ("x" --> "x") `shouldEvalTo` convert True
      bi "isFunction" @@ bi "isFunction" `shouldEvalTo` convert True
      -- TODO test functions that unpack attribute set args
    it "should be false for others" $ do
      bi "isFunction" @@ listE [] `shouldEvalTo` convert False
      bi "isFunction" @@ intE 1 `shouldEvalTo` convert False
  "typeOf" -> wrapDescribe $ do
    let mkTest name e = it ("should work for " <> unpack name) $ do
          bi "typeOf" @@ e `shouldEvalTo` strV name
    mkTest "null" nullE
    mkTest "list" (listE [])
    mkTest "set" (attrsE [])
    mkTest "lambda" ("x" --> "x")
    mkTest "lambda" (bi "typeOf")
    mkTest "int" 1
    mkTest "bool" (boolE False)
  "stringLength" -> wrapDescribe $ do
    it "should get the length" $ property $ \s ->
      bi "stringLength" @@ strE s `shouldEvalTo` convert (length s)
  "attrNames" -> wrapDescribe $ do
    it "should get the names of the set" $ do
      property $ \keys -> do
        let uniqueKeys = S.toList $ S.fromList keys
        -- Make an attribute set from our random list of keys. The
        -- values are irrelevant so just make them null.
        let aset = attrsE $ zip uniqueKeys (repeat nullE)
        -- This test is not quite as clean as it should be, due to the
        -- problem of equality testing requiring ordering.
        Right (VList names) <- evalStrict1 $ bi "attrNames" @@ aset
        sort names `shouldBe` map (pure . strV) (fromList $ sort uniqueKeys)
  "attrValues" -> wrapDescribe $ do
    it "should get the values of the set" $ do
      property $ \constants -> do
        let aset = attrsE $ zip (map tshow [1..]) (map fromConstant constants)
        -- Similarly kind of gross due to order-based equality.
        Right (VList values) <- evalStrict1 $ bi "attrValues" @@ aset
        let eval'dConstants = map (\(Identity (VConstant c)) -> c) values
        sort eval'dConstants `shouldBe` fromList (sort constants)
  "intersectAttrs" -> wrapDescribe $ do
    it "a set intersected with itself should equal itself" $ do
      property $ \constants -> do
        let aset = attrsE $ zip (map tshow [1..]) (map fromConstant constants)
        asetEval'd <- evalStrict1 aset
        intersectedEval'd <- evalStrict1 $ bi "intersectAttrs" @@ aset @@ aset
        asetEval'd `shouldBe` intersectedEval'd
    it "a set intersected with empty should be empty" $ do
      property $ \constants -> do
        let aset = attrsE $ zip (map tshow [1..]) (map fromConstant constants)
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
        bi "hasAttr" @@ strE key @@ attrsE [(key, 1)] `shouldEvalTo` convert True
        bi "hasAttr" @@ strE key @@ attrsE [] `shouldEvalTo` convert False
  "substring" -> wrapDescribe $ do
    it "should take a substring" $ property $ \start len str -> do
      bi "substring" @@ intE start @@ intE len @@ strE str
        `shouldEvalTo` convert (substring start len str)
  "elem" -> wrapDescribe $ do
    it "should find it if it's there" $ do
      -- TODO: randomize the list?
      property $ \const1 const2 const3 -> do
        let list = fromConstants [const1, const2, const3]
        bi "elem" @@ fromConstant const1 @@ list `shouldEvalTo` convert True
        bi "elem" @@ fromConstant const2 @@ list `shouldEvalTo` convert True
        bi "elem" @@ fromConstant const3 @@ list `shouldEvalTo` convert True
    it "shouldn't find it if it isn't there" $ do
      property $ \constant -> do
        let list = listE []
        bi "elem" @@ fromConstant constant @@ list `shouldEvalTo` convert False
  "trace" -> wrapDescribe $ do
    it "should write a message" $ do
      let expr = bi "trace" @@ strE "hello!" @@ 1
      (result, state) <- evalStrict expr
      result `shouldBe` pure (intV 1)
      msWriteBuffer state `shouldBe` fromList ["hello!"]
    it "shouldn't write a message if the expression doesn't get evaluated" $ do
      let traced = bi "trace" @@ strE "hello!" @@ 1
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
        bi "length" @@ fromConstants list
          `shouldEvalTo` fromInt (length list)
      it "shouldn't matter if the list has an error value" $ do
        property $ \list -> do
          bi "length" @@ (listE (failingExpression : map fromConstant list))
            `shouldEvalTo` fromInt (length list + 1)

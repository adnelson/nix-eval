module Nix.Eval.BuiltinsSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Nix.Common
import Nix.Expressions
import Nix.Constants
import Nix.Values
import Nix.Values.NativeConversion
import Nix.Builtins
import Nix.Eval.TestLib
import Nix.Eval.ExpectedBuiltins

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  constantToEnvStringSpec
  valueToEnvStringSpec
  mkTypeTestSpec
  deepSeqSpec
  headSpec
  elemAtSpec
  omapM_ describeTopLevel topLevelKeys
  omapM_ describeBuiltinKey keysInBuiltins

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
      res <- runStrictL $ test (fromConstant constant)
      res `shouldBe` Right (convert True)
  it "should test lists correctly" $ do
    property $ \constantList -> do
      res <- runStrictL $ mkTypeTest RT_List $ fromConstants constantList
      res `shouldBe` Right (convert True)
  it "should test sets correctly" $ do
    property $ \constantSet -> do
      res <- runStrictL $ mkTypeTest RT_AttrSet $ fromConstantSet constantSet
      res `shouldBe` Right (convert True)

deepSeqSpec :: Spec
deepSeqSpec = describe "deepSeq" $ do
  let env = mkEnv [("deepSeq", VNative $ toNative2L builtin_deepSeq),
                   ("throw", VNative $ toNative1 builtin_throw)]
  it "should error on evaluating something that contains an error" $ do
    shouldErrorWithEnv env ("deepSeq" @@ attrsE [("x", failingExpression)]
                                      @@ succeedingExpression)
                           ["CustomError", "failed on purpose"]
  it "should return the second argument if no error" $ do
    shouldEvalWithEnv env ("deepSeq" @@ attrsE [("x", 1)]
                                     @@ succeedingExpression)

headSpec :: Spec
headSpec = describe "list head" $ do
  it "should get the head of a non-empty list" $ do
    let list = listV [fromInt 1]
    res <- runStrictL $ builtin_head list
    res `shouldBe` Right (convert (1 :: Integer))
  it "should error on an empty list" $ do
    builtin_head (listV []) `shouldBeErrorWith` ["IndexError"]

elemAtSpec :: Spec
elemAtSpec = describe "list index" $ do
  it "should index correctly" $ do
    elem <- runStrictL $ builtin_elemAt (listV [intV 1, intV 2]) (fromInt 1)
    elem `shouldBe` Right (convert (2 :: Integer))
  it "should error on an empty list" $ do
    property $ \(i::Int) -> do
      let elem = builtin_elemAt (listV []) (fromInt i)
      elem `shouldBeErrorWith` ["IndexError"]

-- | Defines a bunch of tests for top-level builtin functions.
describeTopLevel :: Text -> Spec
describeTopLevel name = case name of
  "map" -> wrapDescribe mapSpec
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
  "mult" -> wrapSingle $ property $ \i j ->
    bi "mult" @@ intE i @@ intE j `shouldEvalTo` intV (i * j)
  "div" -> wrapSingle $ property $ \i j -> do
    case j of
      0 -> bi "div" @@ intE i @@ intE j `shouldErrorWith` ["DivideByZero"]
      _ -> bi "div" @@ intE i @@ intE j `shouldEvalTo` intV (i `div` j)
  "seq" -> wrapDescribe $ do
    it "should fail if first argument is error" $ do
      property $ \expr ->
        shouldError $ bi "seq" @@ failingExpression @@ expr
    it "should return second argument if first argument succeeds" $ do
      property $ \constant -> do
        let expr = fromConstant constant
        res <- evalStrict expr
        seqRes <- evalStrict (bi "seq" @@ succeedingExpression @@ expr)
        res `shouldBe` seqRes
  "deepSeq" -> deepSeqSpec
  "elemAt" -> elemAtSpec
  "head" -> headSpec
  name -> it "isn't written yet" $ do
    pendingWith $ "No tests yet defined for " <> show name
  where
    bi key = "builtins" !. key
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

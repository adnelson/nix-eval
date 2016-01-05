module Nix.Eval.BuiltinsSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Nix.Common
import Nix.Eval.Expressions
import Nix.Eval.Constants
import Nix.Eval.Values
import Nix.Eval.Builtins
import Nix.Eval.TestLib
import Nix.Eval.ExpectedBuiltins
import Nix.Eval.Evaluator

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  constantToEnvStringSpec
  valueToEnvStringSpec
  mkTypeTestSpec
  omapM_ describeTopLevel topLevelKeys
  omapM_ describeBuiltinKey keysInBuiltins

-- | Convenience function
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
    valueToEnvString (fromConstants strCs) `shouldBe` Right expected
  it "should translate nested lists" $ do
    valueToEnvString (listV ["hey", listV ["yo", "hi"]])
      `shouldBe` Right "hey yo hi"
  it "should not translate sets" $ do
    valueToEnvString (attrsV [("hey", strV "hi")]) `shouldSatisfy` \case
      Left _ -> True
      Right _ -> False

mkTypeTestSpec :: Spec
mkTypeTestSpec = describe "mkTypeTest function" $ do
  it "should test types of constants correctly" $ do
    property $ \constant -> do
      let test = mkTypeTest (typeOf constant)
      test (fromConstant constant) `shouldBe` convert True
  it "should test types of arbitrary values correctly" $ do
    property $ \val rttype -> do
      let test = mkTypeTest rttype
      test val `shouldBe` convert (typeOf val == rttype)
  it "should test lists correctly" $ do
    property $ \constantList -> do
      let test = mkTypeTest RT_List
      test (fromConstants constantList) `shouldBe` convert True
  it "should test sets correctly" $ do
    property $ \constantSet -> do
      let test = mkTypeTest RT_AttrSet
      test (fromConstantSet constantSet) `shouldBe` convert True
  it "should test functions correctly" $ do
    property $ \param closure ->
      mkTypeTest RT_Function (VFunction param closure) `shouldBe` convert True

deepSeqSpec :: Spec
deepSeqSpec = describe "deepSeq" $ do
  it "should error on evaluating something that contains an error" $ do
    (bi "deepSeq" @@ attrsE [("x", failingExpression)]
                  @@ succeedingExpression)
      `shouldErrorWith` ["CustomError", "failed on purpose"]
  it "should return the second argument if no error" $ do
    shouldEval (bi "deepSeq" @@ attrsE [("x", 1)]
                             @@ succeedingExpression)

headSpec :: Spec
headSpec = describe "list head" $ do
  it "should get the head of a non-empty list" $ do
    let list = listE [fromInt 1]
    bi "head" @@ list `shouldEvalTo` convert (1 :: Integer)
  it "should error on an empty list" $ do
    bi "head" @@ (listE []) `shouldErrorWith` ["IndexError"]

elemAtSpec :: Spec
elemAtSpec = describe "list index" $ do
  it "should index correctly" $ do
    builtin_elemAt (listV [intV 1, intV 2]) (fromInt 1)
      `shouldBe` convert (2 :: Integer)
  it "should error on an empty list" $ do
    property $ \(i::Int) -> do
      builtin_elemAt (listV []) (fromInt i)
        `shouldBeErrorWith` ["IndexError"]

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

-- | Defines a bunch of tests for keys appearing in the builtins object.
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
        runEval (bi "seq" @@ succeedingExpression @@ expr)
          `shouldBe` runEval expr
  "deepSeq" -> deepSeqSpec
  "elemAt" -> elemAtSpec
  "head" -> headSpec
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

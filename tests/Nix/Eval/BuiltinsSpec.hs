module Nix.Eval.BuiltinsSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Nix.Common
import Nix.Eval.Expressions
import Nix.Eval.Constants
import Nix.Eval.Values
import Nix.Eval.Builtins
import Nix.Eval.TestLib

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  constantToEnvStringSpec
  valueToEnvStringSpec
  mkTypeTestSpec
  deepSeqSpec

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
  let env = mkEnv [("deepSeq", nativeV builtin_deepSeq),
                   ("throw", nativeV builtin_throw)]
  it "should error on evaluating something that contains an error" $ do
    shouldErrorWithEnv env ("deepSeq" @@ attrsE [("x", failingExpression)]
                                      @@ succeedingExpression)
                           ["CustomError", "failed on purpose"]
  it "should return the second argument if no error" $ do
    shouldEvalWith env ("deepSeq" @@ attrsE [("x", 1)]
                                  @@ succeedingExpression)

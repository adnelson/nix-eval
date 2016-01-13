module Nix.Eval.BuiltinsSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Nix.Common
import Nix.Eval.Expressions
import Nix.Eval.Constants
import Nix.Eval.Values
import Nix.Eval.Values.Builtins
import Nix.Eval.Values.NativeConversion
import Nix.Eval.TestLib

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

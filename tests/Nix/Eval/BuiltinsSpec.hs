module Nix.Eval.BuiltinsSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Nix.Common
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
    valueToEnvString (listV ["hey", "yo"]) `shouldBe` Right "hey yo"
    valueToEnvString (listV ["hey", listV ["yo", "hi"]])
      `shouldBe` Right "hey yo hi"
  it "should not translate sets" $ do
    valueToEnvString (attrsV [("hey", strV "hi")]) `shouldSatisfy` \case
      Left _ -> True
      Right _ -> False

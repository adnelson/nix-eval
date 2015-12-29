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
  constantToStringSpec
  valueToStringSpec
  divisionSpec
  natifySpec

constantToStringSpec :: Spec
constantToStringSpec = describe "constantToString" $ do
  it "should translate strings" $ do
    constantToString "hey" `shouldBe` "hey"
  it "should translate numbers" $ property $
    \i -> constantToString (Int i) `shouldBe` tshow i

valueToStringSpec :: Spec
valueToStringSpec = describe "valueToString" $ do
  it "should translate lists" $ do
    valueToString (listV ["hey", "yo"]) `shouldBe` Right "hey yo"
  it "should translate nested lists" $ do
    valueToString (listV ["hey", "yo"]) `shouldBe` Right "hey yo"
    valueToString (listV ["hey", listV ["yo", "hi"]])
      `shouldBe` Right "hey yo hi"
  it "should not translate sets" $ do
    valueToString (attrsV [("hey", strV "hi")]) `shouldSatisfy` \case
      Left _ -> True
      Right _ -> False

divisionSpec :: Spec
divisionSpec = describe "division" $ do
  let mkInt = validR . intV
  it "should divide numbers" $ do
    applyNative bi_div [mkInt 6, mkInt 3] `shouldBe` mkInt 2
  it "should not divide by zero" $ property $
    \i -> applyNative bi_div [mkInt i, mkInt 0] `shouldBe` errorR DivideByZero

natifySpec :: Spec
natifySpec = describe "natify" $ do
  let badArg = errorR $ CustomError "oh crap"
  describe "when using LazyValues" $ do
    it "shouldn't evaluate unless needed (arity 1)" $ do
      let constFunc = natify $ \(_::LazyValue) -> validR (intV 1)
      applyNative constFunc [badArg] `shouldBe` validR (intV 1)
    it "shouldn't evaluate unless needed (arity 2)" $ do
      let constFunc2 = natify $ \(v::Value) (_::LazyValue) -> validR v
      applyNative constFunc2 [validR nullV, badArg] `shouldBe` validR nullV
  it "SHOULD evaluate even if not needed when using Value" $ do
    let constFunc = natify $ \(_::Value) -> validR (intV 1)
    applyNative constFunc [badArg] `shouldBe` badArg

module Nix.Eval.OperatorsSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Nix.Common
import Nix.Eval.Constants
import Nix.Eval.Values
import Nix.Eval.Operators
import Nix.Eval.TestLib

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  divisionSpec

divisionSpec :: Spec
divisionSpec = describe "division" $ do
  let div_ = natify binop_div
  let mkInt = validR . intV
  it "should divide numbers" $ do
    applyNative div_ [mkInt 6, mkInt 3] `shouldBe` mkInt 2
  it "should not divide by zero" $ property $ \i ->
    applyNative div_ [mkInt i, mkInt 0] `shouldBe` errorR DivideByZero
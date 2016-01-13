module Nix.Eval.OperatorsSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Nix.Common
import Nix.Eval.Constants
import Nix.Eval.Values
import Nix.Eval.Values.Builtins.Operators
import Nix.Eval.TestLib
import Nix.Eval.Values.NativeConversion

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  divisionSpec

divisionSpec :: Spec
divisionSpec = describe "division" $ do
  let div_ = toNative2 binop_div
  let mkInt = pure . intV
  it "should divide numbers" $ do
    res <- runNativeStrictL $ applyNative2 div_ (mkInt 6) (mkInt 3)
    res `shouldBe` Right (intV 2)
  it "should not divide by zero" $ property $ \i -> do
    res <- runNativeStrictL $ applyNative2 div_ (mkInt i) (mkInt 0)
    res `shouldBe` Left DivideByZero

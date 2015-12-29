{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Nix.Eval.BuiltinsSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Nix.Common
import Nix.Eval.Constants
import Nix.Eval.Values
import Nix.Eval.Builtins

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  constantToStringSpec
  valueToStringSpec
  divisionSpec

constantToStringSpec :: Spec
constantToStringSpec = describe "constantToString" $ do
  it "should translate strings" $ do
    constantToString "hey" `shouldBe` "hey"
  it "should translate numbers" $ property $
    \i -> constantToString (Int i) `shouldBe` tshow i

valueToStringSpec :: Spec
valueToStringSpec = describe "valueToString" $ do
  it "should translate lists" $ do
    valueToString (listV ["hey", "yo"]) `shouldBe` validR "hey yo"
  it "should translate nested lists" $ do
    valueToString (listV ["hey", "yo"]) `shouldBe` validR "hey yo"
    valueToString (listV ["hey", listV ["yo", "hi"]])
      `shouldBe` validR "hey yo hi"
  it "should not translate sets" $ do
    valueToString (attrsV [("hey", "hi")]) `shouldSatisfy` isError

divisionSpec :: Spec
divisionSpec = describe "division" $ do
  let mkInt = validR . intV
      Func2 bi_div_ = bi_div
  it "should divide numbers" $ do
    bi_div_ (mkInt 6) (mkInt 3) `shouldBe` mkInt 2
  it "should not divide by zero" $ property $
    \i -> bi_div_ (mkInt i) (mkInt 0) `shouldBe` errorR DivideByZero

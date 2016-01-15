module Nix.Spec.OperatorsSpec (main, spec) where

import Nix.Common
import Nix.Constants
import Nix.Eval.Builtins.Operators
import Nix.Expressions
import Nix.Spec.Lib
import Nix.Values
import Nix.Values.NativeConversion
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  divisionSpec
  binopsSpec
  unopsSpec

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

binopsSpec :: Spec
binopsSpec = describe "binary operators" $ do
  describe "numerical stuff" $ do
    it "should evaluate +" $ property $ \i j ->
      intE i $+ intE j `shouldEvalTo` intV (i + j)
    it "should evaluate -" $ property $ \i j ->
      intE i $- intE j `shouldEvalTo` intV (i - j)
    it "should evaluate *" $ property $ \i j ->
      intE i $* intE j `shouldEvalTo` intV (i * j)
    describe "comparison" $ do
      it "should evaluate <" $ property $ \i j -> do
        fromInteg i $< fromInteg j `shouldEvalTo` fromBool (i < j)
      it "should evaluate <=" $ property $ \i j -> do
        fromInteg i $<= fromInteg j `shouldEvalTo` fromBool (i <= j)
      it "should evaluate >" $ property $ \i j -> do
        fromInteg i $> fromInteg j `shouldEvalTo` fromBool (i > j)
      it "should evaluate >=" $ property $ \i j -> do
        fromInteg i $>= fromInteg j `shouldEvalTo` fromBool (i >= j)
  describe "logic" $ do
    it "should evaluate &&" $ property $ \b1 b2 ->
      boolE b1 $&& boolE b2 `shouldEvalTo` boolV (b1 && b2)
    it "should evaluate ||" $ property $ \b1 b2 ->
      boolE b1 $|| boolE b2 `shouldEvalTo` boolV (b1 || b2)
    it "should evaluate ->" $ property $ \b1 b2 ->
      boolE b1 $-> boolE b2 `shouldEvalTo`
        boolV (if b1 then b2 else True)
  describe "data structures" $ do
    it "should evaluate ++" $ property $ \list1 list2 -> do
      fromConstants list1 $++ fromConstants list2
        `shouldEvalTo` fromConstants (list1 <> list2)
    it "should evaluate //" $ property $ \set1 set2 -> do
      fromConstantSet set1 $// fromConstantSet set2
      `shouldEvalTo` fromConstantSet (set2 <> set1)
    it "should evaluate + for strings" $ property $ \s1 s2 -> do
      strE s1 $+ strE s2 `shouldEvalTo` strV (s1 <> s2)
  describe "equality" $ do
    it "equal things are equal" $ property $ \constant -> do
      fromConstant constant $== fromConstant constant
        `shouldEvalTo` fromBool True
    it "equal things are not unequal" $ property $ \constant -> do
      fromConstant constant $!= fromConstant constant
        `shouldEvalTo` fromBool False
    it "unequal things are unequal" $ property $ \const1 const2 -> do
      fromConstant const1 $== fromConstant const2
        `shouldEvalTo` fromBool (const1 == const2)
    it "unequal things are not equal" $ property $ \const1 const2 -> do
      fromConstant const1 $!= fromConstant const2
        `shouldEvalTo` fromBool (const1 /= const2)

unopsSpec :: Spec
unopsSpec = describe "unary operators" $ do
  describe "numeric negation" $ do
    it "should work once" $ property $ \num -> do
      -(fromInteg num) `shouldEvalTo` fromInteg (-num)
    it "should work twice" $ property $ \num -> do
      -(-(fromInteg num)) `shouldEvalTo` fromInteg num
  describe "logical negation" $ do
    it "should work once" $ property $ \bool -> do
      notE (fromBool bool) `shouldEvalTo` fromBool (not bool)
    it "should work twice" $ property $ \bool -> do
      notE (notE (fromBool bool)) `shouldEvalTo` fromBool bool

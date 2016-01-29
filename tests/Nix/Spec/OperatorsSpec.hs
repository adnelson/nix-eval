module Nix.Spec.OperatorsSpec (main, spec) where

import Nix.Common
import Nix.Atoms
import Nix.Eval
import Nix.Expr
import Nix.Evaluator
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
    res <- runNativeStrictL1 $ applyNative2 div_ (mkInt 6) (mkInt 3)
    res `shouldBe` Right (intV 2)
  it "should not divide by zero" $ property $ \i -> do
    res <- runNativeStrictL1 $ applyNative2 div_ (mkInt i) (mkInt 0)
    res `shouldBe` Left DivideByZero

binopsSpec :: Spec
binopsSpec = describe "binary operators" $ do
  describe "numerical stuff" $ do
    it "should evaluate +" $ property $ \i j ->
      mkInt i $+ mkInt j `shouldEvalTo` intV (i + j)
    it "should evaluate -" $ property $ \i j ->
      mkInt i $- mkInt j `shouldEvalTo` intV (i - j)
    it "should evaluate *" $ property $ \i j ->
      mkInt i $* mkInt j `shouldEvalTo` intV (i * j)
    describe "comparison" $ do
      it "should evaluate <" $ property $ \i j -> do
        mkInt i $< mkInt j `shouldEvalTo` convert (i < j)
      it "should evaluate <=" $ property $ \i j -> do
        mkInt i $<= mkInt j `shouldEvalTo` convert (i <= j)
      it "should evaluate >" $ property $ \i j -> do
        mkInt i $> mkInt j `shouldEvalTo` convert (i > j)
      it "should evaluate >=" $ property $ \i j -> do
        mkInt i $>= mkInt j `shouldEvalTo` convert (i >= j)
  describe "logic" $ do
    it "should evaluate &&" $ property $ \b1 b2 ->
      mkBool b1 $&& mkBool b2 `shouldEvalTo` boolV (b1 && b2)
    it "should evaluate ||" $ property $ \b1 b2 ->
      mkBool b1 $|| mkBool b2 `shouldEvalTo` boolV (b1 || b2)
    it "should evaluate ->" $ property $ \b1 b2 ->
      mkBool b1 $-> mkBool b2 `shouldEvalTo`
        boolV (if b1 then b2 else True)
  describe "data structures" $ do
    it "should evaluate ++" $ property $ \list1 list2 -> do
      fromAtoms list1 $++ fromAtoms list2
        `shouldEvalTo` fromAtoms (list1 <> list2)
    it "should evaluate //" $ property $ \set1 set2 -> do
      fromAtomSet set1 $// fromAtomSet set2
      `shouldEvalTo` fromAtomSet (set2 <> set1)
    it "should evaluate + for strings" $ property $ \s1 s2 -> do
      mkStr s1 $+ mkStr s2 `shouldEvalTo` strV (s1 <> s2)
  describe "equality" $ do
    it "equal things are equal" $ property $ \constant -> do
      fromAtom constant $== fromAtom constant
        `shouldEvalTo` convert True
    it "equal things are not unequal" $ property $ \constant -> do
      fromAtom constant $!= fromAtom constant
        `shouldEvalTo` convert False
    it "unequal things are unequal" $ property $ \const1 const2 -> do
      fromAtom const1 $== fromAtom const2
        `shouldEvalTo` convert (const1 == const2)
    it "unequal things are not equal" $ property $ \const1 const2 -> do
      fromAtom const1 $!= fromAtom const2
        `shouldEvalTo` convert (const1 /= const2)

unopsSpec :: Spec
unopsSpec = describe "unary operators" $ do
  describe "numeric negation" $ do
    it "should work once" $ property $ \num -> do
      -(mkInt num) `shouldEvalTo` convert (-num)
    it "should work twice" $ property $ \num -> do
      -(-(mkInt num)) `shouldEvalTo` convert num
  describe "logical negation" $ do
    it "should work once" $ property $ \bool -> do
      mkNot (mkBool bool) `shouldEvalTo` convert (not bool)
    it "should work twice" $ property $ \bool -> do
      mkNot (mkNot (mkBool bool)) `shouldEvalTo` convert bool

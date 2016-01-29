module Nix.Spec.EvaluatorSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck (property)
import Nix.Expr
import Nix.Common
import Nix.Atoms
import Nix.Values hiding (WHNFValue, LazyValue)
import Nix.Spec.Lib as Lib
import Nix.Evaluator (builtin_throw, builtin_seq, binop_div, EvalError(..))
import Nix.Values.NativeConversion
import qualified Data.Map as M
import qualified Data.Set as S

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  testBalloonSpec
  functionsSpec
  lazyEvalSpec
  builtinAppSpec
  attrSetSpec
  withSpec
  letSpec
  listSpec

-- | Ensure that the failing expression fails, and succeeding
-- expression succeeds.
testBalloonSpec :: Spec
testBalloonSpec = describe "test-balloon expressions" $ do
  it "should fail to evaluate failing expression" $ do
    shouldError failingExpression
  it "should evaluate succeeding expression" $ do
    shouldEval succeedingExpression

functionsSpec :: Spec
functionsSpec = describe "functions" $ do
  it "should evaluate the identity function" $ do
    shouldEvalToWithEnv emptyE ("x" ==> "x")
                               (functionV (Param "x") (emptyC "x"))
  it "should evaluate nested functions" $ do
    shouldEvalToWithEnv emptyE ("x" ==> "y" ==> "x")
                               (functionV (Param "x")
                                          (emptyC ("y" ==> "x")))
  it "should evaluate function applications" $ do
    let expr = ("x" ==> "x") @@ mkStr "hello"
    expr `shouldEvalTo` "hello"
  it "should capture environment in closure" $ do
    let env = mkEnv [("x", intV 1)]
    shouldEvalToWithEnv env (("foo" ==> "x") @@ mkInt 2) (intV 1)
  describe "unpacking arguments" $ do
    describe "without defaults" $ do
      let mkParams = FixedParamSet . M.fromList . map (\p -> (p, Nothing))
      let params = mkParams ["foo", "bar"]
      let func = ParamSet params Nothing ==> "foo" + "bar"
      it "should unpack attribute sets" $ do
        func @@ attrsE [("foo", 1), ("bar", 2)] `shouldEvalTo` intV 3
      it "should fail if argument is not a set" $ do
        func @@ 1 `shouldErrorWith` ["TypeError"]
      it "should fail if an argument is missing" $ do
        func @@ attrsE [("foo", 1)]
          `shouldErrorWith` ["MissingArguments", "bar"]
      it "should report all missing arguments" $ do
        Left (MissingArguments args) <- evalStrict1 (func @@ attrsE [])
        S.fromList args `shouldBe` S.fromList ["foo", "bar"]
      it "should allow assigning the argument to a variable" $ do
        let func = ParamSet params (Just "args") ==> ("args" !. "foo")
        func @@ attrsE [("foo", 1), ("bar", mkNull)] `shouldEvalTo` intV 1
      it "should fail if extra args passed to fixed param set" $ do
        func @@ attrsE [("foo", 1), ("bar", 2), ("baz", 3)]
          `shouldErrorWith` ["ExtraArguments", "baz"]
    describe "default arguments" $ do
      let params = FixedParamSet $ M.fromList [("foo", Nothing),
                                               ("bar", Just 1)]
          func = ParamSet params Nothing ==> "foo" + "bar"
      it "should allow default arguments" $ do
        func @@ attrsE [("foo", 2)] `shouldEvalTo` intV 3
      it "should let the default arguments be overridden" $ do
        func @@ attrsE [("foo", 2), ("bar", 5)] `shouldEvalTo` intV 7
      it "should allow defaults to refer to captured variables" $ do
        let params = FixedParamSet $ M.fromList [("foo", Nothing),
                                                 ("bar", Just ("blaz" + 2))]
        let func = letE "blaz" 6 $
                     mkFunction (ParamSet params Nothing) ("foo" + "bar")
        func @@ attrsE [("foo", 4)] `shouldEvalTo` intV 12
      it "should allow defaults to refer to other arguments" $ do
        let params = FixedParamSet $ M.fromList [("foo", Nothing),
                                                 ("bar", Just ("foo" + 2))]
        let func = mkFunction (ParamSet params Nothing) ("foo" + "bar")
        func @@ attrsE [("foo", 4)] `shouldEvalTo` intV 10
      it "should allow defaults to refer to the arguments name" $ do
        let params = FixedParamSet $ M.fromList [("foo", Just "args"),
                                                 ("bar", Nothing)]
        let func = mkFunction (ParamSet params (Just "args"))
                     (("foo" !. "bar") + 2)
        pendingWith "getting infinite loops here :("
        func @@ attrsE [("bar", 9)] `shouldEvalTo` intV 11

-- | Test the evaluation of builtins (not the builtins themselves, but
-- that an arbitrary builtin is correctly evaluated)
builtinAppSpec :: Spec
builtinAppSpec = describe "application of builtins" $ do
  it "should work with unary builtins" $ do
    -- Make an ID function builtin, and try it out.
    let my_id = toNative1 $ \v -> pure v
        env = mkEnv [("id", VNative my_id)]
    shouldEvalToWithEnv env ("id" @@ 1) (intV 1)
  it "should work with a const builtin" $ do
    -- Make a const function builtin, and try it.
    let my_const = toNative2L $ \v (_::LazyValue) -> pure v
        env = mkEnv [("const", VNative my_const)]
        shouldEvalTo' = shouldEvalToWithEnv env
    ("const" @@ 1 @@ 2) `shouldEvalTo'` intV 1
  it "should work with a div builtin" $ do
    -- Add the division function builtin, and try it.
    let env = mkEnv [("div", VNative $ toNative2 binop_div)]
        shouldEvalTo' = shouldEvalToWithEnv env
    ("div" @@ 10 @@ 2) `shouldEvalTo'` intV 5

-- | This test more or less checks that our expressions are evaluated
-- lazily; that is, that there is no need to evaluate an expression
-- before passing it into a function.
lazyEvalSpec :: Spec
lazyEvalSpec = describe "lazy evaluation" $ do
  let errE = "throw" @@ mkStr "oh no!"
  it "should not evaluate a function argument unless needed" $ do
    -- Make a function which ignores its argument (just returns "1").
    let constFunc = "_" ==> mkInt 1
     -- The constant function should ignore an error argument.
    constFunc @@ errE `shouldEvalTo` intV 1
  it "should short-circuit logical AND" $ do
    -- Evaluation should return without triggering the error.
    let expr = mkBool False $&& errE
    expr `shouldEvalTo` boolV False
  it "should short-circuit logical OR" $ do
    -- Evaluation should return without triggering the error.
    let expr = mkBool True $|| errE
    expr `shouldEvalTo` boolV True
  -- Test the `toNative` class of functions.
  describe "toNative" $ do
    let err = CustomError "oh crap"
    describe "when using lazy values" $ do
      it "shouldn't evaluate unless needed (arity 1)" $ do
        -- Even though we're passing an error argument to this
        -- function, it should still return 1 since it's lazy.
        let constFunc = toNative1L $ \(_::LazyValue) -> pure (intV 1)
        res <- runNativeStrictL1 $ applyNative constFunc (throwError err)
        res `shouldBe` Right (intV 1)
      it "shouldn't evaluate unless needed (arity 2)" $ do
        let constFunc2 = toNative2L $ \(v::WHNFValue) (_::LazyValue) -> pure v
        res <- runNativeStrictL1 $
                 applyNative2 constFunc2 (pure nullV) (throwError err)
        res `shouldBe` pure nullV
    describe "when using strict values" $ do
      it "SHOULD evaluate even if not needed when using WHNFValue" $ do
        let constFunc = toNative1 $ \(_::WHNFValue) -> pure (intV 1)
        res <- runNativeStrictL1 $ applyNative constFunc (throwError err)
        res `shouldBe` Left err

attrSetSpec :: Spec
attrSetSpec = describe "attribute sets" $ do
  describe "non-recursive" $ do
    it "should evaluate attr set literals" $ do
      let mySet = attrsE [("x", 1)]
      mySet `shouldEvalTo` attrsV [("x", intV 1)]
    it "should access set members" $ do
      let mySet = attrsE [("x", 1)]
      mySet !. "x" `shouldEvalTo` intV 1
    it "should access nested set members" $ do
      let mySet = attrsE [("x", (attrsE [("y", 1)]))]
      mySet !. "x" !. "y" `shouldEvalTo` intV 1
    it "should not have a problem with error members unless accessed" $ do
      -- Create an attribute set in which one of the members causes an
      -- error when evaluated.
      let mySet = attrsE [("good", mkStr "hello"),
                          ("bad", "undefined-variable")]
      mySet !. "good" `shouldEvalTo` strV "hello"
      mySet !. "bad" `shouldErrorWith` ["NameError", "undefined-variable"]
    it "should throw a KeyError if key doesn't exist" $ do
      attrsE [] !. "x" `shouldErrorWith` ["KeyError", "x"]
  describe "recursive" $ do
    it "should evaluate attr set literals" $ do
      let mySet = recAttrsE [("x", 1)]
      shouldEvalToWithEnv emptyE mySet $ attrsV [("x", intV 1)]
    it "should access set members" $ do
      let mySet = recAttrsE [("x", 1)]
      mySet !. "x" `shouldEvalTo` intV 1
    it "should access nested set members" $ do
      -- This value is
      -- rec {x = {y = x; z = 2};}
      -- So accessing mySet.x.y.z should give 2.
      let mySet = recAttrsE [("x", attrsE [("y", "x"),
                                           ("z", 2)])]
      mySet !. "x" !. "y" !. "z" `shouldEvalTo` intV 2
    it "should allow inter-references in the set" $ do
      let mySet = recAttrsE [("x", 1), ("y", "x")]
      mySet !. "y" `shouldEvalTo` intV 1
    it "should detect infinite loops" $ do
      let infiniteSet = recAttrsE [("x", "y"), ("y", "x")]
      pendingWith "we're not detecting these yet"
      infiniteSet !. "y" `shouldErrorWith` ["InfiniteRecursion"]

withSpec :: Spec
withSpec = describe "with expressions" $ do
  it "should introduce variables" $ do
    let mySet = attrsE [("x", 1)]
    mkWith mySet "x" `shouldEvalTo` intV 1
  it "should introduce variables from recursive sets" $ do
    let mySet = recAttrsE [("x", 1), ("y", "x")]
    mkWith mySet "x" `shouldEvalTo` intV 1
    mkWith mySet "y" `shouldEvalTo` intV 1
  it "should not propagate environment into the record" $ do
    let expr = letsE
                 -- Introduce a variable `x`.
                 [("x", 1),
                 -- make a variable `r` which doesn't have `x`.
                  ("r", recAttrsE [])]
                 -- Attempt to evaluate `r.x`.
                 ("r" !. "x")
    -- This should result in a key error.
    expr `shouldErrorWith` ["KeyError", "x"]
  it "should not have a problem with error variables unless accessed" $ do
    let mySet = attrsE [("x", 1), ("fail", failingExpression)]
    mkWith mySet "x" `shouldEvalTo` intV 1
    shouldError $ mkWith mySet "fail"

letSpec :: Spec
letSpec = describe "let conversion" $ do
  it "should introduce a variable" $ property $ \constant -> do
    letE "x" (fromAtom constant) "x" `shouldEvalTo` fromAtom constant
  it "should let variables reference each other" $ do
    property $ \constant -> do
      let expr = letsE [("x", fromAtom constant),
                        ("y", "x")] "y"
      expr `shouldEvalTo` fromAtom constant

listSpec :: Spec
listSpec = describe "lists" $ do
  it "should eval an empty list" $ do
    mkList [] `shouldEvalTo` listV []
  it "should eval a list with a few elements" $ do
    mkList [1, 2, 3] `shouldEvalTo` listV (map intV [1, 2, 3])
  describe "operations on lists" $ do
    it "should map a function over a list" $ property $ \list ->
      "map" @@ ("x" ==> "x") @@ fromAtoms list
        `shouldEvalTo` fromAtoms list
    it "should map the +1 function over a list" $ property $ \nums -> do
        "map" @@ ("x" ==> "x" + 1) @@ (fromAtoms $ map NInt nums)
          `shouldEvalTo` fromAtoms (map (\i -> NInt (i + 1)) nums)
    it "should map over a nested list" $ property $ \nums1 nums2 -> do
      let list1 = fromAtoms $ map NInt nums1
          list2 = fromAtoms $ map NInt nums2
          list3 = mkList [list1, list2]
      -- The ID function over a nested list
      "map" @@ ("x" ==> "x") @@ list3
        `shouldEvalTo` listV (map (fromAtoms . map NInt) [nums1, nums2])
      -- Mapping the map function
      "map" @@ ("map" @@ ("x" ==> "x")) @@ list3
        `shouldEvalTo` listV [fromAtoms $ map NInt nums1,
                              fromAtoms $ map NInt nums2]
      -- Mapping functions which do things
      "map" @@ ("map" @@ ("x" ==> "x" * 2)) @@ list3
        `shouldEvalTo` listV [fromAtoms $ map (NInt . (*2)) nums1,
                              fromAtoms $ map (NInt . (*2)) nums2]
    describe "length" $ do
      it "should get the length of a list" $ property $ \list ->
        "length" @@ fromAtoms list
          `shouldEvalTo` convert (length list)
      it "shouldn't matter if the list has an error value" $ do
        property $ \list -> do
          "length" @@ (mkList (failingExpression : map fromAtom list))
            `shouldEvalTo` convert (length list + 1)

module Nix.Eval.EvaluatorSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck (property)
import Nix.Common
import Nix.Constants
import Nix.Values
import Nix.Expressions
import Nix.Eval.TestLib
import Nix.Builtins.NativeFunctions (builtin_throw, builtin_seq)
import Nix.Builtins.Operators (binop_div)
import Nix.Values.NativeConversion

main :: IO ()
main = hspec spec

spec :: Spec
spec = do --describe "something" $ it "something" $ True `shouldBe` True
  testBalloonSpec
  functionsSpec
  binopsSpec
  lazyEvalSpec
  builtinAppSpec
  attrSetSpec
  withSpec
  letSpec
  listSpec
  unopsSpec

-- | Ensure that the failing expression fails, and succeeding
-- expression succeeds.
testBalloonSpec :: Spec
testBalloonSpec = describe "test-balloon expressions" $ do
  it "should fail to evaluate failing expression" $ do
    shouldError failingExpression
  it "should evaluate succeeding expression" $ do
    shouldEval succeedingExpression

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


functionsSpec :: Spec
functionsSpec = describe "functions" $ do
  it "should evaluate the identity function" $ do
    shouldEvalToWithEnv emptyE ("x" --> "x") (functionV "x" (emptyC "x"))
  it "should evaluate nested functions" $ do
    shouldEvalToWithEnv emptyE ("x" --> "y" --> "x")
                               (functionV "x" (emptyC (ELambda "y" "x")))
  it "should evaluate function applications" $ do
    let expr = ("x" --> "x") @@ (strE "hello")
    expr `shouldEvalTo` "hello"
  it "should capture environment in closure" $ do
    let env = mkEnv [("x", intV 1)]
    shouldEvalToWithEnv env (("foo" --> "x") @@ intE 2) (intV 1)

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
  let errE = "throw" @@ strE "oh no!"
  it "should not evaluate a function argument unless needed" $ do
    -- Make a function which ignores its argument (just returns "1").
    let constFunc = "_" --> intE 1
     -- The constant function should ignore an error argument.
    constFunc @@ errE `shouldEvalTo` intV 1
  it "should short-circuit logical AND" $ do
    -- Evaluation should return without triggering the error.
    let expr = boolE False `andE` errE
    expr `shouldEvalTo` boolV False
  it "should short-circuit logical OR" $ do
    -- Evaluation should return without triggering the error.
    let expr = boolE True `orE` errE
    expr `shouldEvalTo` boolV True
  -- Test the `toNative` class of functions.
  describe "toNative" $ do
    let err = CustomError "oh crap"
    describe "when using lazy values" $ do
      it "shouldn't evaluate unless needed (arity 1)" $ do
        -- Even though we're passing an error argument to this
        -- function, it should still return 1 since it's lazy.
        let constFunc = toNative1L $ \(_::LazyValue) -> pure (intV 1)
        res <- runNativeStrictL $ applyNative constFunc (throwError err)
        res `shouldBe` Right (intV 1)
      it "shouldn't evaluate unless needed (arity 2)" $ do
        let constFunc2 = toNative2L $ \(v::WHNFValue) (_::LazyValue) -> pure v
        res <- runNativeStrictL $
                 applyNative2 constFunc2 (pure nullV) (throwError err)
        res `shouldBe` pure nullV
    describe "when using strict values" $ do
      it "SHOULD evaluate even if not needed when using WHNFValue" $ do
        let constFunc = toNative1 $ \(_::WHNFValue) -> pure (intV 1)
        res <- runNativeStrictL $ applyNative constFunc (throwError err)
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
      let mySet = attrsE [("good", strE "hello"),
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
    withE mySet "x" `shouldEvalTo` intV 1
  it "should introduce variables from recursive sets" $ do
    let mySet = recAttrsE [("x", 1), ("y", "x")]
    withE mySet "x" `shouldEvalTo` intV 1
    withE mySet "y" `shouldEvalTo` intV 1
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
    withE mySet "x" `shouldEvalTo` intV 1
    shouldError $ withE mySet "fail"

letSpec :: Spec
letSpec = describe "let conversion" $ do
  it "should introduce a variable" $ property $ \constant -> do
    letE "x" (fromConstant constant) "x" `shouldEvalTo` fromConstant constant
  it "should let variables reference each other" $ do
    property $ \constant -> do
      let expr = letsE [("x", fromConstant constant),
                        ("y", "x")] "y"
      expr `shouldEvalTo` fromConstant constant

listSpec :: Spec
listSpec = describe "lists" $ do
  it "should eval an empty list" $ do
    listE [] `shouldEvalTo` listV []
  it "should eval a list with a few elements" $ do
    listE [1, 2, 3] `shouldEvalTo` listV (map intV [1, 2, 3])
  describe "operations on lists" $ do
    it "should map a function over a list" $ property $ \list ->
      "map" @@ ("x" --> "x") @@ fromConstants list
        `shouldEvalTo` fromConstants list
    it "should map the +1 function over a list" $ property $ \nums -> do
        "map" @@ ("x" --> "x" + 1) @@ (fromConstants $ map Int nums)
          `shouldEvalTo` fromConstants (map (\i -> Int (i + 1)) nums)
    it "should map over a nested list" $ property $ \nums1 nums2 -> do
      let list1 = fromConstants $ map Int nums1
          list2 = fromConstants $ map Int nums2
          list3 = listE [list1, list2]
      -- The ID function over a nested list
      "map" @@ ("x" --> "x") @@ list3
        `shouldEvalTo` listV (map (fromConstants . map Int) [nums1, nums2])
      -- Mapping the map function
      "map" @@ ("map" @@ ("x" --> "x")) @@ list3
        `shouldEvalTo` listV [fromConstants $ map Int nums1,
                              fromConstants $ map Int nums2]
      -- Mapping functions which do things
      "map" @@ ("map" @@ ("x" --> "x" * 2)) @@ list3
        `shouldEvalTo` listV [fromConstants $ map (Int . (*2)) nums1,
                              fromConstants $ map (Int . (*2)) nums2]
    describe "length" $ do
      it "should get the length of a list" $ property $ \list ->
        "length" @@ fromConstants list
          `shouldEvalTo` fromInt (length list)
      it "shouldn't matter if the list has an error value" $ do
        property $ \list -> do
          "length" @@ (listE (failingExpression : map fromConstant list))
            `shouldEvalTo` fromInt (length list + 1)

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

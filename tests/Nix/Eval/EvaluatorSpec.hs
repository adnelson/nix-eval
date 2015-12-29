module Nix.Eval.EvaluatorSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck (property)
import Nix.Common
import Nix.Eval.Constants
import Nix.Eval.Values
import Nix.Eval.Expressions
import Nix.Eval.TestLib
import Nix.Eval.Builtins

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  failureSpec
  functionsSpec
  binopsSpec
  lazyEvalSpec
  builtinAppSpec
  attrSetSpec
  withSpec
  listSpec

-- | Ensure that the failing expression fails.
failureSpec :: Spec
failureSpec = describe "failing expression" $ do
  it "should fail to evaluate" $ do
    shouldError failingExpression

binopsSpec :: Spec
binopsSpec = describe "binary operators" $ do
  it "should evaluate +" $ property $
    \i j -> (intE i + intE j) `shouldEvalTo` intV (i + j)
  it "should evaluate + for strings" $ do
    strE "hey" + strE "yo" `shouldEvalTo` strV "heyyo"
  it "should evaluate -" $ property $ \i j ->
    intE i - intE j `shouldEvalTo` intV (i - j)
  it "should evaluate *" $ property $ \i j ->
    intE i * intE j `shouldEvalTo` intV (i * j)
  it "should evaluate &&" $ property $ \b1 b2 ->
    boolE b1 `andE` boolE b2 `shouldEvalTo` boolV (b1 && b2)
  it "should evaluate ||" $ property $ \b1 b2 ->
    boolE b1 `orE` boolE b2 `shouldEvalTo` boolV (b1 || b2)
  it "should evaluate ++" $ do
    let list1 = map Int [1, 2, 3]
        list2 = map Int [4, 5, 6]
    let conc = listE (map fromConstant list1) + listE (map fromConstant list2)
    conc `shouldEvalTo` listV (map fromConstant $ list1 <> list2)

functionsSpec :: Spec
functionsSpec = describe "functions" $ do
  it "should evaluate the identity function" $ do
    ("x" --> "x") `shouldEvalTo` functionV "x" (emptyC "x")
  it "should evaluate nested functions" $ do
    ("x" --> "y" --> "x")
      `shouldEvalTo` functionV "x" (emptyC (ELambda "y" "x"))
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
    let bi_id = natify $ \v -> validR v
        env = mkEnv [("id", VNative bi_id)]
    shouldEvalToWithEnv env ("id" @@ 1) (intV 1)
  it "should work with a const builtin" $ do
    -- Make a const function builtin, and try it.
    let bi_const = natify $ \v (_::LazyValue) -> validR v
        env = mkEnv [("const", VNative bi_const)]
        shouldEvalTo' = shouldEvalToWithEnv env
    ("const" @@ 1 @@ 2) `shouldEvalTo'` intV 1
  it "should work with a div builtin" $ do
    -- Add the division function builtin, and try it.
    let env = mkEnv [("div", VNative bi_div)]
        shouldEvalTo' = shouldEvalToWithEnv env
    ("div" @@ 10 @@ 2) `shouldEvalTo'` intV 5

-- | This test more or less checks that our expressions are evaluated
-- lazily; that is, that there is no need to evaluate an expression
-- before passing it into a function.
lazyEvalSpec :: Spec
lazyEvalSpec = describe "lazy evaluation" $ do
  -- Introduce builtin "throw" function.
  let env = mkEnv [("throw", VNative bi_throw),
                   ("seq", VNative bi_seq)]
      errE = "throw" @@ strE "oh no!"
      shouldEvalTo' = shouldEvalToWithEnv env
  it "should not evaluate a function argument unless needed" $ do
    -- Make a function which ignores its argument (just returns "1").
    let constFunc = "_" --> intE 1
     -- The constant function should ignore an error argument.
    (constFunc @@ errE) `shouldEvalTo'` intV 1
  it "should short-circuit logical AND" $ do
    -- Evaluation should return without triggering the error.
    let expr = boolE False `andE` errE
    expr `shouldEvalTo'` boolV False
  it "should short-circuit logical OR" $ do
    -- Evaluation should return without triggering the error.
    let expr = boolE True `orE` errE
    expr `shouldEvalTo'` boolV True

attrSetSpec :: Spec
attrSetSpec = describe "attribute sets" $ do
  describe "non-recursive" $ do
    it "should evaluate attr set literals" $ do
      let mySet = attrsE [("x", 1)]
      mySet `shouldEvalTo` attrsV [("x", intV 1)]
    it "should access set members" $ do
      let mySet = attrsE [("x", 1)]
      mySet !. "x" `shouldEvalTo` intV 1
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
      mySet `shouldEvalTo` attrsV [("x", intV 1)]
    it "should access set members" $ do
      let mySet = recAttrsE [("x", 1)]
      mySet !. "x" `shouldEvalTo` intV 1
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
  it "should not have a problem with error variables unless accessed" $ do
    let mySet = attrsE [("x", 1), ("fail", failingExpression)]
    withE mySet "x" `shouldEvalTo` intV 1
    shouldError $ withE mySet "fail"

listSpec :: Spec
listSpec = describe "lists" $ do
  it "should eval an empty list" $ do
    listE [] `shouldEvalTo` listV []
  it "should eval a list with a few elements" $ do
    listE [1, 2, 3] `shouldEvalTo` listV (map intV [1, 2, 3])
  describe "operations on lists" $ do
    it "should map a function over a list" $ do
      pending
    describe "length" $ do
      it "should get the length of a list" $ do
        pending
      it "shouldn't matter if the list has an error value" $ do
        pending

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
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
  functionsSpec
  binopsSpec
  lazyEvalSpec
  builtinAppSpec
  attrSetSpec

binopsSpec :: Spec
binopsSpec = describe "binary operators" $ do
  it "should evaluate +" $ property $
    \i j -> (intE i + intE j) `shouldEvalTo` intV (i + j)
  it "should evaluate + for strings" $ do
    (strE "hey" + strE "yo") `shouldEvalTo` strV "heyyo"
  it "should evaluate -" $ property $
    \i j -> (intE i - intE j) `shouldEvalTo` intV (i - j)
  it "should evaluate *" $ property $
    \i j -> (intE i * intE j) `shouldEvalTo` intV (i * j)
  it "should evaluate &&" $ property $
    \b1 b2 -> (boolE b1 `andE` boolE b2) `shouldEvalTo` boolV (b1 && b2)
  it "should evaluate ||" $ property $
    \b1 b2 -> (boolE b1 `orE` boolE b2) `shouldEvalTo` boolV (b1 || b2)

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
    let bi_id = lazify $ \v -> validR v
        env = mkEnv [("id", VBuiltin "id" bi_id)]
        shouldEvalTo' = shouldEvalToWithEnv env
    ("id" @@ 1) `shouldEvalTo'` intV 1
  it "should work with a const builtin" $ do
    -- Make a const function builtin, and try it.
    let bi_const = lazify2 $ \v _ -> validR v
        env = mkEnv [("const", VBuiltin2 "const" bi_const)]
        shouldEvalTo' = shouldEvalToWithEnv env
    ("const" @@ 1 @@ 2) `shouldEvalTo'` intV 1
  it "should work with a div builtin" $ do
    -- Add the division function builtin, and try it.
    let env = mkEnv [("div", VBuiltin2 "div" bi_div)]
        shouldEvalTo' = shouldEvalToWithEnv env
    ("div" @@ 10 @@ 2) `shouldEvalTo'` intV 5

-- | This test more or less checks that our expressions are evaluated
-- lazily; that is, that there is no need to evaluate an expression
-- before passing it into a function.
lazyEvalSpec :: Spec
lazyEvalSpec = describe "lazy evaluation" $ do
  -- Introduce builtin "throw" function.
  let env = mkEnv [("throw", VBuiltin "throw" bi_throw),
                   ("seq", VBuiltin2 "seq" bi_seq)]
      errE = "throw" @@ strE "oh no!"
      shouldEvalTo' = shouldEvalToWithEnv env
  it "should not evaluate a function argument unless needed" $ do
    let
      -- A function which ignores its argument (just returns "1").
      constFunc = "_" --> intE 1
     -- The constant function should ignore an error argument.
    (constFunc @@ errE) `shouldEvalTo'` intV 1
  it "should short-circuit logical AND and OR" $ do
    let
      expr1 = boolE False `andE` errE
      expr2 = boolE True `orE` errE
    -- Evaluation should return without triggering the error.
    expr1 `shouldEvalTo'` boolV False
    expr2 `shouldEvalTo'` boolV True

attrSetSpec :: Spec
attrSetSpec = describe "attribute sets" $ do
  it "should evaluate attr set literals" $ do
    let mySet = attrsE [("x", 1)]
    mySet `shouldEvalTo` attrsV [("x", intV 1)]
  it "should access set members" $ do
    let mySet = attrsE [("x", 1)]
    (mySet !. "x") `shouldEvalTo` intV 1
  it "should not have a problem with error members unless accessed" $ do
    -- Create an attribute set in which one of the members causes an
    -- error when evaluated.
    let mySet = attrsE [("good", strE "hello"),
                        ("bad", "non-existent-variable")]
    (mySet !. "good") `shouldEvalTo` strV "hello"
    (mySet !. "bad") `shouldErrorWith` NameError "non-existent-variable" emptyE
  it "should throw a KeyError if key doesn't exist" $ do
    attrsE [] !. "x" `shouldErrorWith` KeyError "x" (mkEnv [])

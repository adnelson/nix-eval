module Nix.Eval.TestLib where

import Data.Either
import Test.Hspec
import Test.QuickCheck hiding (Result)
import Nix.Common
import Nix.Eval.Expressions
import Nix.Eval.Evaluator
import Nix.Eval.Constants
import Nix.Eval.Values
import Nix.Eval.Builtins

shouldEvalTo :: Expression -> Value -> Expectation
shouldEvalTo = shouldEvalToWithEnv emptyE

infixl 0 `shouldEvalTo`

shouldEvalToWithEnv :: Environment -> Expression -> Value -> Expectation
shouldEvalToWithEnv env expr val = evaluate env expr
                                   `shouldBe` (validR val)

shouldErrorWith :: Expression -> EvalError -> Expectation
shouldErrorWith = shouldErrorWithEnv emptyE

infixl 0 `shouldErrorWith`

shouldErrorWithEnv :: Environment -> Expression -> EvalError -> Expectation
shouldErrorWithEnv env expr err = evaluate env expr
                                   `shouldBe` (errorR err)


shouldBeValid :: Show a => Result a -> Expectation
shouldBeValid res = shouldSatisfy res $ \case
  Result (Left _) -> False
  _ -> True

shouldBeError :: Show a => Result a -> Expectation
shouldBeError res = shouldSatisfy res $ \case
  Result (Left _) -> True
  _ -> False

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

shouldEvalToWithEnv :: Environment -> Expression -> Value -> Expectation
shouldEvalToWithEnv env expr val = evaluate env expr
                                   `shouldBe` (Result $ Right val)

shouldBeValid :: Result Value -> Expectation
shouldBeValid res = shouldSatisfy res isValid

shouldBeError :: Result Value -> Expectation
shouldBeError res = shouldSatisfy res isError

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

shouldError :: Expression -> Expectation
shouldError expr = shouldBeError $ evaluate allBuiltins expr

shouldErrorWith :: Expression -> [String] -> Expectation
shouldErrorWith = shouldErrorWithEnv allBuiltins

infixl 0 `shouldErrorWith`

shouldErrorWithEnv :: Environment -> Expression -> [String] -> Expectation
shouldErrorWithEnv env expr strings =
  evaluate env expr `shouldSatisfy` \(Result res) -> case res of
    Left err -> all (`isInfixOf` show err) strings
    _ -> False

shouldBeValid :: Show a => Result a -> Expectation
shouldBeValid res = shouldSatisfy res $ \case
  Result (Left _) -> False
  _ -> True

shouldBeError :: Show a => Result a -> Expectation
shouldBeError res = shouldSatisfy res $ \case
  Result (Left _) -> True
  _ -> False

shouldBeNameError :: Show a => Result a -> Expectation
shouldBeNameError res = shouldSatisfy res $ \case
  Result (Left (NameError _ _)) -> True
  _ -> False


-- | An expression that will always fail to evaluate.
failingExpression :: Expression
failingExpression = "throw" @@ strE "this expression should fail"

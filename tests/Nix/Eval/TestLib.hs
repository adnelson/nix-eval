{-# OPTIONS_GHC -fno-warn-orphans #-}
module Nix.Eval.TestLib where

import Data.Either
import Test.Hspec
import Test.QuickCheck hiding (Result)
import Nix.Common
import Nix.Types (NBinaryOp(..))
import Nix.Eval.Expressions
import Nix.Eval.Evaluator
import Nix.Eval.Constants
import Nix.Eval.Values
import Nix.Eval.Builtins
import qualified Data.HashMap.Strict as H
import qualified Data.Set as S

-- | Make an orphan Text instance; not sure why this isn't there already...
instance Arbitrary Text where
  arbitrary = pack <$> arbitrary

-- | Similarly for hashmaps
instance (Arbitrary k, Eq k, Hashable k, Arbitrary v)
         => Arbitrary (HashMap k v) where
  arbitrary = H.fromList <$> arbitrary

instance Arbitrary a => Arbitrary (Seq a) where
  arbitrary = fromList <$> arbitrary

instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
  arbitrary = S.fromList <$> arbitrary

instance Arbitrary Constant where
  arbitrary = oneof
    [ String <$> arbitrary
    , Path . fromString . (\(i::Int) -> "/foo/bar" <> show i) <$> arbitrary
    , Int <$> arbitrary
    , Bool <$> arbitrary
    , pure Null ]

-- | We'll make arbitrary expressions but only containing constants,
-- so that we don't get nameerrors and the like.
instance Arbitrary Expression where
  arbitrary = oneof
    [ EConstant <$> arbitrary
    , EList <$> arbitrary ]

instance Arbitrary Value where
  arbitrary = oneof
    [ VConstant <$> arbitrary
    , VAttrSet <$> arbitrary
    , VList <$> arbitrary
    , VFunction <$> arbitrary <*> arbitrary
    , VNative <$> arbitrary ]

instance Arbitrary Environment where
  arbitrary = Environment <$> arbitrary

instance Arbitrary Closure where
  arbitrary = Closure <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (Result a) where
  arbitrary = frequency
    [ (10, Result . Right <$> arbitrary)
    , (1, Result . Left <$> arbitrary) ]

instance Arbitrary Native where
  arbitrary = NativeValue <$> arbitrary

instance Arbitrary EvalError where
  arbitrary = oneof
    [ NameError <$> arbitrary <*> arbitrary
    , KeyError <$> arbitrary <*> arbitrary
    , TypeError <$> arbitrary <*> arbitrary
    , pure DivideByZero
    , CustomError <$> arbitrary
    , pure InfiniteRecursion
    , pure AssertionError
    ]

instance Arbitrary RuntimeType where
  arbitrary = oneof $ map pure $ enumFrom RT_Null

shouldEvalTo :: Expression -> Value -> Expectation
shouldEvalTo expr val = runEval expr `shouldBe` validR val

infixl 0 `shouldEvalTo`

shouldEvalToWithEnv :: Environment -> Expression -> Value -> Expectation
shouldEvalToWithEnv env expr val = evaluate env expr
                                   `shouldBe` (validR val)

shouldError :: Expression -> Expectation
shouldError expr = shouldBeError $ runEval expr

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

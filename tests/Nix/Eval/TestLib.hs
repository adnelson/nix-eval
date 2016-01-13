{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Nix.Eval.TestLib where

import Data.Either
import Test.Hspec
import Test.QuickCheck hiding (Result)
import Nix.Common
import Nix.Types (NBinaryOp(..))
import Nix.Eval
import Nix.Eval.Expressions
import Nix.Eval.Evaluator
import Nix.Eval.Constants
import Nix.Eval.Values
import Nix.Eval.Values.Builtins
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

instance Monad m => Arbitrary (Value m) where
  arbitrary = oneof
    [ VConstant <$> arbitrary
    , VAttrSet <$> arbitrary
    , VList <$> map (map return) arbitrary
    , VFunction <$> arbitrary <*> arbitrary
    , VNative . NativeValue . return <$> arbitrary ]

instance Monad m => Arbitrary (Environment m) where
  arbitrary = Environment <$> map (map return) arbitrary

instance Monad m => Arbitrary (Closure m) where
  arbitrary = Closure <$> arbitrary <*> arbitrary

instance Monad m => Arbitrary (Native m (Value m)) where
  arbitrary = NativeValue <$> map return arbitrary

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

runStrict :: Expression -> IO (Either EvalError StrictValue)
runStrict expr = run $ lazyToStrict $ performEval expr

runStrictWithEnv :: LEnvironment -> Expression ->
                    IO (Either EvalError StrictValue)
runStrictWithEnv env expr = run $ lazyToStrict $ evaluate env expr

runNativeStrict :: LNative WHNFValue ->
                   IO (Either EvalError StrictValue)
runNativeStrict = run . lazyToStrict . unwrapNative

runNativeStrictL :: Eval (LNative WHNFValue) ->
                   IO (Either EvalError StrictValue)
runNativeStrictL lazy = run $ lazyToStrict . unwrapNative =<< lazy


shouldEvalTo :: Expression -> StrictValue -> Expectation
shouldEvalTo expr val = do
  result <- run $ lazyToStrict $ performEval expr
  result `shouldBe` pure val

shouldEvalToWithEnv :: LEnvironment -> Expression -> StrictValue -> Expectation
shouldEvalToWithEnv env expr val = do
  result <- run $ lazyToStrict $ evaluate env expr
  result `shouldBe` pure val

shouldBeError :: Show a => Eval a -> Expectation
shouldBeError action = do
  res <- run action
  shouldSatisfy res $ \case
    Left _ -> True
    _ -> False

shouldBeNameError :: Show a => Eval a -> Expectation
shouldBeNameError action = do
  res <- run action
  shouldSatisfy res $ \case
    Left (NameError _ _) -> True
    _ -> False


shouldBeErrorWith :: Show a => Eval a -> [String] -> Expectation
shouldBeErrorWith action strings = do
  res <- run action
  shouldSatisfy res $ \case
    Left err -> all (`isInfixOf` show err) strings
    _ -> False

-- | An expression that will always fail to evaluate.
failingExpression :: Expression
failingExpression = "throw" @@ strE "failed on purpose"

-- | An expression that will always succeed evaluation.
succeedingExpression :: Expression
succeedingExpression = strE "success"

shouldBeValid :: Show a => Eval a -> Expectation
shouldBeValid action = do
  res <- run action
  shouldSatisfy res $ \case
    Left _ -> False
    _ -> True

shouldErrorWithEnv :: LEnvironment -> Expression -> [String] -> Expectation
shouldErrorWithEnv env expr strings = do
  res <- run $ lazyToStrict $ evaluate env expr
  res `shouldSatisfy` \case
    Left err -> all (`isInfixOf` show err) strings
    _ -> False

shouldEval :: Expression -> Expectation
shouldEval expr = shouldBeValid $ lazyToStrict $ performEval expr

shouldEvalWith :: LEnvironment -> Expression -> Expectation
shouldEvalWith env expr = shouldBeValid $ lazyToStrict $ evaluate env expr

infixl 0 `shouldEvalTo`

shouldError :: Expression -> Expectation
shouldError expr = shouldBeError $ lazyToStrict $ performEval expr

shouldErrorWith :: Expression -> [String] -> Expectation
shouldErrorWith = shouldErrorWithEnv allBuiltins

infixl 0 `shouldErrorWith`

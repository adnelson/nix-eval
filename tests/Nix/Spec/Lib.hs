{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Nix.Spec.Lib where

import Data.Either
import Control.Monad.State.Strict
import Test.Hspec
import Test.QuickCheck hiding (Result)
import Nix.Types (Formals(..))
import Nix.Common
import Nix.Types (NBinaryOp(..))
import Nix.Eval hiding (WHNFValue, LazyValue, LEnvironment,
                        LNative, LClosure)
import qualified Data.HashMap.Strict as H
import qualified Data.Set as S
import qualified Nix.Eval as Eval

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
    , VFunction <$> (FormalName <$> arbitrary) <*> arbitrary
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
  arbitrary = oneof $ pure <$> enumFrom RT_Null

-- | We can use this to mock out things like filesystem interaction
-- and message writing.
data MockState = MockState {
    msWriteBuffer :: Seq Text
  } deriving (Show, Eq)

-- | The monad we're using to test things in.
type TestM = StateT MockState IO

instance WriteMessage TestM where
  writeMessage msg = modify $ \s -> s {
    msWriteBuffer = msWriteBuffer s `snoc` msg
    }

type WHNFValue = Eval.WHNFValue TestM
type LazyValue = Eval.LazyValue TestM
type LNative = Eval.LNative TestM
type LEnvironment = Eval.LEnvironment TestM
type LAttrSet = Eval.LAttrSet TestM
type LClosure = Eval.LClosure TestM

defaultMock :: MockState
defaultMock = MockState {
  msWriteBuffer = mempty
  }

runMock = flip runStateT defaultMock . run
evalMock = map fst . runMock

runStrict :: WHNFValue -> IO (Either EvalError StrictValue, MockState)
runStrict = runMock . whnfToStrict

runStrict1 :: WHNFValue -> IO (Either EvalError StrictValue)
runStrict1 = map fst . runStrict

runStrict2 :: WHNFValue -> IO MockState
runStrict2 = map snd . runStrict

runStrictL :: LazyValue -> IO (Either EvalError StrictValue, MockState)
runStrictL = runMock . lazyToStrict

runStrictL1 :: LazyValue -> IO (Either EvalError StrictValue)
runStrictL1 = map fst . runStrictL

runStrictL2 :: LazyValue -> IO MockState
runStrictL2 = map snd . runStrictL

evalStrict :: Expression -> IO (Either EvalError StrictValue, MockState)
evalStrict = runMock . lazyToStrict . performEval

evalStrict1 :: Expression -> IO (Either EvalError StrictValue)
evalStrict1 = map fst . evalStrict

evalStrict2 :: Expression -> IO MockState
evalStrict2 = map snd . evalStrict

evalStrictWithEnv :: LEnvironment -> Expression ->
                    IO (Either EvalError StrictValue, MockState)
evalStrictWithEnv env expr = runMock $ lazyToStrict $ evaluate env expr

runNativeStrict :: LNative WHNFValue ->
                   IO (Either EvalError StrictValue, MockState)
runNativeStrict lnative = runMock $ lazyToStrict $ unwrapNative lnative

runNativeStrict1 :: LNative WHNFValue -> IO (Either EvalError StrictValue)
runNativeStrict1 = map fst . runNativeStrict

runNativeStrict2 :: LNative WHNFValue -> IO MockState
runNativeStrict2 = map snd . runNativeStrict

runNativeStrictL :: Eval TestM (LNative WHNFValue) ->
                   IO (Either EvalError StrictValue, MockState)
runNativeStrictL lazy = runMock $ lazyToStrict . unwrapNative =<< lazy

runNativeStrictL1 :: Eval TestM (LNative WHNFValue) ->
                     IO (Either EvalError StrictValue)
runNativeStrictL1 = map fst . runNativeStrictL

runNativeStrictL2 :: Eval TestM (LNative WHNFValue) -> IO MockState
runNativeStrictL2 = map snd . runNativeStrictL

shouldEvalTo :: Expression -> StrictValue -> Expectation
shouldEvalTo expr val = do
  result <- evalMock $ lazyToStrict $ performEval expr
  result `shouldBe` pure val

shouldEvalToWithEnv :: LEnvironment -> Expression -> StrictValue -> Expectation
shouldEvalToWithEnv env expr val = do
  result <- evalMock $ lazyToStrict $ evaluate env expr
  result `shouldBe` pure val

shouldBeError :: LazyValue -> Expectation
shouldBeError action = do
  res <- evalMock $ lazyToStrict action
  shouldSatisfy res $ \case
    Left _ -> True
    _ -> False

shouldBeNameError :: LazyValue -> Expectation
shouldBeNameError action = do
  res <- evalMock $ lazyToStrict action
  shouldSatisfy res $ \case
    Left (NameError _ _) -> True
    _ -> False

shouldBeErrorWith :: LazyValue -> [String] -> Expectation
shouldBeErrorWith action strings = do
  res <- evalMock $ lazyToStrict action
  shouldSatisfy res $ \case
    Left err -> all (`isInfixOf` show err) strings
    _ -> False

-- | An expression that will always fail to evaluate.
failingExpression :: Expression
failingExpression = "throw" @@ strE "failed on purpose"

-- | An expression that will always succeed evaluation.
succeedingExpression :: Expression
succeedingExpression = strE "success"

shouldBeValid :: Show a => Eval TestM a -> Expectation
shouldBeValid action = do
  res <- evalMock action
  shouldSatisfy res $ \case
    Left _ -> False
    _ -> True

shouldErrorWithEnv :: LEnvironment -> Expression -> [String] -> Expectation
shouldErrorWithEnv env expr strings = do
  res <- evalMock $ lazyToStrict $ evaluate env expr
  res `shouldSatisfy` \case
    Left err -> all (`isInfixOf` show err) strings
    _ -> False

shouldEval :: Expression -> Expectation
shouldEval expr = shouldBeValid $ lazyToStrict $ performEval expr

shouldEvalWithEnv :: LEnvironment -> Expression -> Expectation
shouldEvalWithEnv env expr = shouldBeValid $ lazyToStrict $ evaluate env expr

infixl 0 `shouldEvalTo`

shouldError :: Expression -> Expectation
shouldError expr = shouldBeError $ performEval expr

shouldErrorWith :: Expression -> [String] -> Expectation
shouldErrorWith = shouldErrorWithEnv topLevelBuiltins

infixl 0 `shouldErrorWith`

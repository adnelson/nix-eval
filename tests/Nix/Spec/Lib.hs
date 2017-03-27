{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Nix.Spec.Lib where

import Data.Either
import Control.Monad.State.Strict
import Test.Hspec
import Test.QuickCheck hiding (Result, (==>))
import Nix.Atoms
import Nix.Expr (Params(..))
import Nix.Common
import Nix.Expr
import Nix.Evaluator hiding (WHNFValue, LazyValue, LEnvironment,
                             LNative, LClosure)
import qualified Data.HashMap.Strict as H
import qualified Data.Set as S
import qualified Nix.Evaluator as Eval

-- | Make an orphan Text instance; not sure why this isn't there already...
instance Arbitrary Text where
  arbitrary = pack <$> arbitrary

-- | Similarly for hashmaps
instance (Arbitrary k, Eq k, Hashable k, Arbitrary v)
         => Arbitrary (HashMap k v) where
  arbitrary = H.fromList <$> arbitrary

instance Arbitrary NAtom where
  arbitrary = oneof
    [ NInt <$> arbitrary
    , NBool <$> arbitrary
    , NUri <$> arbitrary
    , pure NNull ]

-- | We'll make arbitrary expressions but only containing constants,
-- so that we don't get nameerrors and the like.
instance Arbitrary NExpr where
  arbitrary = oneof
    [ Fix . NConstant <$> arbitrary ]

instance Monad m => Arbitrary (Value m) where
  arbitrary = oneof
    [ VConstant <$> arbitrary
    , VAttrSet <$> arbitrary
    , VList <$> map (map return) arbitrary
    , VFunction <$> (Param <$> arbitrary) <*> arbitrary
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

-- | NExprs can be parsed from numbers.
instance Num NExpr where
  fromInteger = mkInt
  e1@(Fix (NList _)) + e2 = mkBinop NConcat e1 e2
  e1 + e2 = mkBinop NPlus e1 e2
  e1 - e2 = mkBinop NMinus e1 e2
  e1 * e2 = mkBinop NMult e1 e2
  negate = Fix . NUnary NNeg
  abs = error "No absolute value for Nix expressions"
  signum = error "No sign for Nix expressions"

instance FromAtom NExpr where
  fromAtom = Fix . NConstant
  fromAtoms = mkList . map fromAtom
  fromAtomSet = attrsE . map (map fromAtom) . H.toList

-- | Give a more specific type than `convert`, to prevent ambiguity.
convertI :: FromAtom t => Integer -> t
convertI = convert

-- | We can use this to mock out things like filesystem interaction
-- and message writing.
data MockState = MockState {
    msWriteBuffer :: Seq Text
  } deriving (Show, Eq)

-- | The monad we're using to test things in.
type TestM = StateT MockState IO

-- | For our test implementation of 'WriteMessage', we just collect
-- all written messages into the state's write buffer (which is just a
-- list of strings).
instance WriteMessage TestM where
  writeMessage msg = modify $ \s -> s {
    msWriteBuffer = msWriteBuffer s `snoc` msg
    }

instance Nix TestM

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

runMock :: Eval TestM a -> IO (Either EvalError a, MockState)
runMock = flip runStateT defaultMock . run

runMock1 :: Eval TestM a -> IO (Either EvalError a)
runMock1 = map fst . runMock

runMock2 :: Eval TestM a -> IO MockState
runMock2 = map snd . runMock

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

evalStrict :: NExpr -> IO (Either EvalError StrictValue, MockState)
evalStrict = runMock . lazyToStrict . performEval

evalStrict1 :: NExpr -> IO (Either EvalError StrictValue)
evalStrict1 = map fst . evalStrict

evalStrict2 :: NExpr -> IO MockState
evalStrict2 = map snd . evalStrict

evalStrictWithEnv :: LEnvironment -> NExpr ->
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

shouldEvalTo :: NExpr -> StrictValue -> Expectation
shouldEvalTo expr val = do
  result <- runMock1 $ lazyToStrict $ performEval expr
  result `shouldBe` pure val

shouldEvalToWithEnv :: LEnvironment -> NExpr -> StrictValue -> Expectation
shouldEvalToWithEnv env expr val = do
  result <- runMock1 $ lazyToStrict $ evaluate env expr
  result `shouldBe` pure val

shouldBeError :: LazyValue -> Expectation
shouldBeError action = do
  res <- runMock1 $ lazyToStrict action
  shouldSatisfy res $ \case
    Left _ -> True
    _ -> False

shouldBeNameError :: LazyValue -> Expectation
shouldBeNameError action = do
  res <- runMock1 $ lazyToStrict action
  shouldSatisfy res $ \case
    Left (NameError _ _) -> True
    _ -> False

shouldBeErrorWith :: LazyValue -> [String] -> Expectation
shouldBeErrorWith action strings = do
  res <- runMock1 $ lazyToStrict action
  shouldSatisfy res $ \case
    Left err -> all (`isInfixOf` show err) strings
    _ -> False

-- | An expression that will always fail to evaluate.
failingExpression :: NExpr
failingExpression = "throw" @@ mkStr "failed on purpose"

-- | An expression that will always succeed evaluation.
succeedingExpression :: NExpr
succeedingExpression = mkStr "success"

shouldBeValid :: Show a => Eval TestM a -> Expectation
shouldBeValid action = do
  res <- runMock1 action
  shouldSatisfy res $ \case
    Left _ -> False
    _ -> True

shouldErrorWithEnv :: LEnvironment -> NExpr -> [String] -> Expectation
shouldErrorWithEnv env expr strings = do
  res <- runMock1 $ lazyToStrict $ evaluate env expr
  res `shouldSatisfy` \case
    Left err -> all (`isInfixOf` show err) strings
    _ -> False

shouldEval :: NExpr -> Expectation
shouldEval expr = shouldBeValid $ lazyToStrict $ performEval expr

shouldEvalWithEnv :: LEnvironment -> NExpr -> Expectation
shouldEvalWithEnv env expr = shouldBeValid $ lazyToStrict $ evaluate env expr

infixl 0 `shouldEvalTo`

shouldError :: NExpr -> Expectation
shouldError expr = shouldBeError $ performEval expr

shouldErrorWith :: NExpr -> [String] -> Expectation
shouldErrorWith = shouldErrorWithEnv topLevelBuiltins

infixl 0 `shouldErrorWith`

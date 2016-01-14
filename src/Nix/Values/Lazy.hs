module Nix.Values.Lazy where

import Nix.Common
import Nix.Expressions
import Nix.Constants
import Nix.Values.Generic
import qualified Data.HashMap.Strict as H

-- | The result of evaluation: it might be an error.
newtype Eval a = Eval {runEval :: ExceptT EvalError IO a}
  deriving (Functor, Applicative, Monad)

-- | Run an evaluator action, and return either an error or a result.
run :: Eval a -> IO (Either EvalError a)
run = runExceptT . runEval

instance MonadError EvalError Eval where
  throwError = Eval . throwError
  catchError action handler = Eval $ do
    runEval action `catchError` \err -> runEval $ handler err

-- | Weak-head-normal-form values are strict at the top-level, but
-- internally contains lazily evaluated values.
type WHNFValue = Value Eval

-- | This is how we represent a lazily evaluated value: It's an
-- 'Eval', which means it might be an error; but if it's not an
-- error it will be a value in WHNF.
type LazyValue = Eval WHNFValue

instance ShowIO a => ShowIO (Eval a) where
  showIO (Eval a) = showIO a

instance ShowIO WHNFValue where
  showIO (VConstant c) = return $ "VConstant (" <> tshow c <> ")"
  showIO (VAttrSet set) = showIO set
  showIO (VList vs) = do
    inners <- mapM showIO vs
    return $ concat ["[", intercalate ", " $ toList inners, "]"]
  showIO (VFunction param closure) = do
    closureRep <- showIO closure
    return $ concat [ param, " => (", closureRep, ")"]
  showIO (VNative (NativeValue v)) = showIO v
  showIO (VNative _) = return "(native function)"

-- | We use a nix-esque syntax for showing an environment.
instance ShowIO LEnvironment where
  showIO (Environment env) = do
    items <- showItems
    return ("{" <> items <> "}")
    where
      showPair (n, v) = showIO v >>= \v' -> return (n <> " = " <> v')
      showItems = intercalate "; " <$> mapM showPair (H.toList env)

instance ShowIO LClosure where
  showIO (Closure env body) = do
    envRep <- showIO env
    return $ "with " <> envRep <> "; " <> tshow body

-- Some type synonyms for readability. The 'L' is for lazy.
type LNative = Native Eval
type LEnvironment = Environment Eval
type LAttrSet = AttrSet Eval
type LClosure = Closure Eval

-------------------------------------------------------------------------------
--------------------------- Deep Evaluation -----------------------------------
-------------------------------------------------------------------------------

-- | Most of the time we evaluate things lazily, but sometimes we want
-- to be able to evaluate strictly (esp. in the `deepSeq` function).
deeplyEval :: WHNFValue -> LazyValue
deeplyEval val = case val of
  VNative (NativeValue nval) -> join $ deeplyEvalLazy nval
  VList lvals -> map VList $ mapM deeplyEvalLazy lvals
  VAttrSet (Environment attrs) -> do
    VAttrSet . Environment <$> mapM deeplyEvalLazy attrs
  -- Note that we don't recur into functions (or NativeFunctions), and
  -- since constants can't fail there's no need to handle them either.
  _ -> return val

-- | Deeply evaluate a lazy value, while keeping it appearing as lazy.
deeplyEvalLazy :: LazyValue -> Eval LazyValue
deeplyEvalLazy lval = lval >>= deeplyEval >> return lval

-- instance FromConstant LazyValue where
--   fromConstant = pure . fromConstant
--   fromConstants = pure . fromConstants
--   fromConstantSet = pure . fromConstantSet

-- | Synonym for monadic bind, applying a function inside of a
-- 'Result', provided the 'Result' contains a 'WHNFValue'.
unwrapAndApply :: (WHNFValue -> LazyValue) -> LazyValue -> LazyValue
unwrapAndApply func res = res >>= func

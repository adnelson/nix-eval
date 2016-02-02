-- | Describes lazily-evaluated values, for which the context is one
-- in which both errors and filesystem interaction are possible.
module Nix.Values.Lazy where

import Nix.Common
import Nix.Evaluator.Errors
import Nix.Evaluator.RuntimeTypes
import Nix.Evaluator.Contexts (WriteMessage(..))
import Nix.Values.Generic
import qualified Data.HashMap.Strict as H
import Control.Monad.Fix (MonadFix(..))

-- | The result of evaluation: it might be an error.
newtype Eval m a = Eval {runEval :: ExceptT EvalError m a}
  deriving (Functor, Applicative, Monad)

-- | Run an evaluator action, and return either an error or a result.
run :: Eval m a -> m (Either EvalError a)
run = runExceptT . runEval

instance Monad m => MonadError EvalError (Eval m) where
  throwError = Eval . throwError
  catchError action handler = Eval $ do
    runEval action `catchError` \err -> runEval $ handler err

instance WriteMessage m => WriteMessage (Eval m) where
  writeMessage :: Text -> Eval m ()
  writeMessage msg = do
    Eval $ ExceptT $ map Right $ writeMessage msg

-- | Weak-head-normal-form values are strict at the top-level, but
-- internally contains lazily evaluated values.
type WHNFValue m = Value (Eval m)

-- | This is how we represent a lazily evaluated value: It's an
-- 'Eval', which means it might be an error; but if it's not an
-- error it will be a value in WHNF.
type LazyValue m = Eval m (WHNFValue m)

-- instance ShowIO a => ShowIO (Eval a) where
--   showIO (Eval a) = showIO a

-- instance ShowIO WHNFValue where
--   showIO (VConstant c) = return $ "VConstant (" <> tshow c <> ")"
--   showIO (VAttrSet set) = showIO set
--   showIO (VList vs) = do
--     inners <- mapM showIO vs
--     return $ concat ["[", intercalate ", " $ toList inners, "]"]
--   showIO (VFunction param closure) = do
--     closureRep <- showIO closure
--     return $ concat [ param, " => (", closureRep, ")"]
--   showIO (VNative (NativeValue v)) = showIO v
--   showIO (VNative _) = return "(native function)"

-- -- | We use a nix-esque syntax for showing an environment.
-- instance ShowIO LEnvironment where
--   showIO (Environment env) = do
--     items <- showItems
--     return ("{" <> items <> "}")
--     where
--       showPair (n, v) = showIO v >>= \v' -> return (n <> " = " <> v')
--       showItems = intercalate "; " <$> mapM showPair (H.toList env)

-- instance ShowIO LClosure where
--   showIO (Closure env body) = do
--     envRep <- showIO env
--     return $ "with " <> envRep <> "; " <> tshow body

-- Some type synonyms for readability. The 'L' is for lazy.
type LNative m = Native (Eval m)
type LEnvironment m = Environment (Eval m)
type LAttrSet m = AttrSet (Eval m)
type LClosure m = Closure (Eval m)

-------------------------------------------------------------------------------
--------------------------- Deep Evaluation -----------------------------------
-------------------------------------------------------------------------------

-- | Most of the time we evaluate things lazily, but sometimes we want
-- to be able to evaluate strictly (esp. in the `deepSeq` function).
deeplyEval :: Monad m => WHNFValue m -> LazyValue m
deeplyEval val = case val of
  VNative (NativeValue nval) -> join $ deeplyEvalLazy nval
  VList lvals -> map VList $ mapM deeplyEvalLazy lvals
  VAttrSet (Environment attrs) -> do
    VAttrSet . Environment <$> mapM deeplyEvalLazy attrs
  -- Note that we don't recur into functions (or NativeFunctions), and
  -- since constants can't fail there's no need to handle them either.
  _ -> return val

-- | Deeply evaluate a lazy value, while keeping it appearing as lazy.
deeplyEvalLazy :: Monad m => LazyValue m -> Eval m (LazyValue m)
deeplyEvalLazy lval = lval >>= deeplyEval >> return lval

-- instance FromConstant LazyValue where
--   fromConstant = pure . fromConstant
--   fromConstants = pure . fromConstants
--   fromConstantSet = pure . fromConstantSet

-- | Synonym for monadic bind, applying a function inside of a
-- 'Result', provided the 'Result' contains a 'WHNFValue'.
unwrapAndApply :: Monad m =>
                  (WHNFValue m -> LazyValue m) ->
                  LazyValue m -> LazyValue m
unwrapAndApply func res = res >>= func

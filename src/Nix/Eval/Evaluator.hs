{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
module Nix.Eval.Evaluator where

import Nix.Common
import Nix.Eval.Errors
import Nix.Eval.Expressions
import Nix.Eval.Values
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H

-- -- | Evaluate a thunk. Returns immediately if the thunk contains a Right;
-- -- otherwise evaluates the thunk and returns the result.
-- evalThunk :: Thunk expr val -- ^ Thunk to evaluate.
--           -> IO (Value expr val) -- ^ Result of evaluation (WHNF).
-- evalThunk (Thunk ref) = readIORef ref >>= \case
--   Right evaluated -> return evaluated
--   Left (Closure env expr) -> do
--     result <- evaluateWHNF env expr
--     writeIORef ref $ Right result
--     return result

-- evalApplication :: Environment expr val -- ^ Enclosing environment.
--                 -> Text                 -- ^ Function parameter name.
--                 -> Closure expr val     -- ^ Function environment.
--                 -> Value expr val       -- ^ Function argument.
--                 -> IO (Value expr val)  -- ^ Result of the application.
-- evalApplication env param closure argument = do
--   -- The union function is left-biased, so the closure's env shadows that of
--   -- the calling environment, and both are shadowed by the argument.
--   let newEnv = singleEnv param argument `unionEnv` cEnv closure `unionEnv` env
--   -- Evaluate the function body with this new environment.
--   evaluateWHNF newEnv $ cBody closure

-- -- | Evaluate an expression within an environment to WHNF.
-- evaluateWHNF :: Environment expr val -- ^ Enclosing environment.
--          -> Expression expr      -- ^ Expression to evaluate.
--          -> IO (Value expr val)  -- ^ Result of evaluation.
-- evaluateWHNF env expr = case expr of
--   EConstant const -> return $ vConstant const
--   EVar name -> case lookupEnv name env of
--     Nothing -> throwEvalError $ NameError name
--     Just val -> return val
--   -- Application is lazily evaluated
--   -- EApply _ _ -> Value <$> mkThunk env expr

--   ELet bindings expr' -> undefined
--   _ -> error "Not defined yet"

-- showVal :: Value e v -> IO Text
-- showVal (VConstant const) = return $ tshow const
-- showVal (VAttrSet _) = undefined

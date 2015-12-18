{-# LANGUAGE NoImplicitPrelude #-}
module Nix.Eval.Errors where

import Nix.Common
import Nix.Eval.Expressions
import Nix.Eval.Values
import Nix.Eval.RuntimeTypes

data EvalError
  = NameError Text
  | TypeError RuntimeType RuntimeType
  deriving (Show, Eq, Typeable)

instance Exception EvalError

throwEvalError :: EvalError -> IO a
throwEvalError = throwIO

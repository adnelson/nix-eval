{-# LANGUAGE NoImplicitPrelude #-}
module Nix.Eval.Errors where

import Nix.Common
import Nix.Eval.Expressions
import Nix.Eval.Values
import Nix.Eval.RuntimeTypes

data EvalError
  = NameError Text
  | TypeError RuntimeType RuntimeType
  | BuiltinError Text Text
  deriving (Show, Eq, Typeable)

instance Exception EvalError

type Result = Either EvalError

throwEvalError :: EvalError -> IO a
throwEvalError = throwIO

throwPure :: EvalError -> Either EvalError a
throwPure = Left

throwTypeError :: RuntimeType -> Value -> Either EvalError a
throwTypeError expected val =
  throwPure $ TypeError expected $ typeOfValue val

module Nix.Eval (
  module Nix.Common,
  module Nix.Constants,
  module Nix.Expressions,
  module Nix.Values,
  module Nix.Eval.Evaluator,
  module Nix.Eval.Builtins,
  performEval
  ) where

import Nix.Common
import Nix.Constants hiding (fromText)
import Nix.Expressions
import Nix.Values
import Nix.Eval.Evaluator
import Nix.Eval.Builtins

-- | Evaluate an expression with the builtins in scope.
performEval :: Monad m => Expression -> LazyValue m
performEval = evaluate topLevelBuiltins

module Nix.Eval (
  module Nix.Common,
  module Nix.Constants,
  module Nix.Expressions,
  module Nix.Values,
  module Nix.Eval.Evaluator,
  module Nix.Eval.Builtins,
  module Nix.Eval.RuntimeTypes,
  module Nix.Eval.Errors,
  performEval
  ) where

import Nix.Common
import Nix.Constants hiding (fromText)
import Nix.Expressions
import Nix.Values
import Nix.Eval.Evaluator
import Nix.Eval.Builtins
import Nix.Eval.Errors
import Nix.Eval.RuntimeTypes

-- | Evaluate an expression with the builtins in scope.
performEval :: Nix m => Expression -> LazyValue m
performEval = evaluate topLevelBuiltins

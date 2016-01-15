module Nix.Eval (
  module Nix.Constants,
  module Nix.Expressions,
  module Nix.Values,
  module Nix.Eval.Evaluator,
  module Nix.Eval.Builtins
  ) where

import Nix.Constants
import Nix.Expressions
import Nix.Values
import Nix.Eval.Evaluator
import Nix.Eval.Builtins

-- | Evaluate an expression with the builtins in scope.
performEval :: Expression -> LazyValue
performEval = evaluate topLevelBuiltins

module Nix.Evaluator (
  module Nix.Common,
  module Nix.Constants,
  module Nix.Expressions,
  module Nix.Values,
  module Nix.Evaluator.Evaluator,
  module Nix.Evaluator.Builtins,
  module Nix.Evaluator.RuntimeTypes,
  module Nix.Evaluator.Errors,
  performEval
  ) where

import Nix.Common
import Nix.Constants hiding (fromText)
import Nix.Expressions
import Nix.Values
import Nix.Evaluator.Contexts (Nix)
import Nix.Evaluator.Evaluator
import Nix.Evaluator.Builtins
import Nix.Evaluator.Errors
import Nix.Evaluator.RuntimeTypes

-- | Evaluate an expression with the builtins in scope.
performEval :: Nix m => Expression -> LazyValue m
performEval = evaluate topLevelBuiltins

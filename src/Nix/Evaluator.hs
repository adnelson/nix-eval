module Nix.Evaluator (
  module Nix.Common,
  module Nix.Values,
  module Nix.Evaluator.Contexts,
  module Nix.Evaluator.Evaluator,
  module Nix.Evaluator.Builtins,
  module Nix.Evaluator.RuntimeTypes,
  module Nix.Evaluator.Errors,
  module Nix.Atoms,
  module Nix.Expr,
  performEval
  ) where

import Nix.Common
import Nix.Values
import Nix.Atoms
import Nix.Expr
import Nix.Evaluator.Contexts (Nix, WriteMessage(..))
import Nix.Evaluator.Evaluator
import Nix.Evaluator.Builtins
import Nix.Evaluator.Errors
import Nix.Evaluator.RuntimeTypes

-- | Evaluate an expression with the builtins in scope.
performEval :: Nix m => NExpr -> LazyValue m
performEval = evaluate topLevelBuiltins

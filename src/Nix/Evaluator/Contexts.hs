{-# LANGUAGE ConstraintKinds #-}
-- | Abstract evaluation contexts
-- Not every monad can act as an evaluation context for Nix
-- expressions. In particular, there are certain primitive
-- side-effects we must support, such as printing to screen for the
-- `trace` builtin, or reading the contents of a directory.
-- So we have a more specialized type class here, which is actually
-- made up of a few more specific type classes.
module Nix.Evaluator.Contexts where

import Control.Monad.Fix (MonadFix)

import Nix.Common
import Nix.Evaluator.Errors

-- | A monad in which we can print messages. This lets us implement
-- the `trace` builtin function.
class Monad m => WriteMessage m where
  -- | Write a message (e.g. to stdout)
  writeMessage :: Text -> m ()

-- | The instance for IO is just printing to stdout.
instance WriteMessage IO where
  writeMessage = putStrLn

-- | Combination of all of the different type classes that a type must
-- satisfy in order to be able to evaluate nix expressions.
type Nix m = (WriteMessage m, MonadFix m)

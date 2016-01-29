-- | Abstract evaluation contexts
-- Not every monad can act as an evaluation context for Nix
-- expressions. In particular, there are certain primitive
-- side-effects we must support, such as printing to screen for the
-- `trace` builtin, or reading the contents of a directory.
-- So we have a more specialized type class here, which is actually
-- made up of a few more specific type classes.
module Nix.Evaluator.Contexts where

import Nix.Common
import Nix.Evaluator.Errors

-- | A monad in which we can print messages. This lets us implement
-- the `trace` builtin function.
class Monad m => WriteMessage m where
  -- | Write a message (e.g. to stdout)
  writeMessage :: Text -> m ()

-- | This class doesn't express any of its own methods; rather it just
-- wraps several other type classes into one.
class (WriteMessage m) => Nix m

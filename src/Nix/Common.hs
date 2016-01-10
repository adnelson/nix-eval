{-# LANGUAGE NoImplicitPrelude #-}
module Nix.Common (
  module ClassyPrelude,
  module Data.Text,
  module Data.HashMap.Strict,
  module Data.Fix,
  module Filesystem.Path.CurrentOS,
  module Data.Sequence,
  module GHC.Generics,
  module Control.DeepSeq,
  Extract(..),
  pathToText
  ) where

import ClassyPrelude hiding (assert, asList, find, FilePath, bracket,
                             maximum, maximumBy, (</>), ($>),
                             minimum, try, stripPrefix, ioError,
                             mapM_, sequence_, forM_,
                             filterM, replicateM, writeFile, readFile)
import Data.Text (Text)
import qualified Data.Text as T
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Data.Sequence (Seq)
import Control.Monad.State.Strict (StateT, MonadState(..))
import Data.Fix
import GHC.Generics
import Control.DeepSeq (NFData(..))
import Filesystem.Path.CurrentOS hiding (concat, null, (<.>), empty)

-- | Convert a FilePath into Text.
pathToText :: FilePath -> Text
pathToText pth = case toText pth of
  Left p -> p
  Right p -> p

(.>) :: (a -> b) -> (b -> c) -> (a -> c)
(.>) = flip (.)

-- | The opposite of 'pure'; classes whose internal values can be
-- extracted purely.
class Extract m where
  extract :: m a -> a

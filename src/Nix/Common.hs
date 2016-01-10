{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Nix.Common (
  module ClassyPrelude,
  module Data.Text,
  module Data.HashMap.Strict,
  module Data.Fix,
  module Filesystem.Path.CurrentOS,
  module Data.Sequence,
  module GHC.Generics,
  module Control.DeepSeq,
  module Control.Monad.Identity,
  module Control.Monad.Except,
  Extract(..), ShowIO(..),
  pathToText
  ) where

import           ClassyPrelude              hiding (FilePath, asList, assert,
                                             bracket, filterM, find, forM_,
                                             ioError, mapM_, maximum, maximumBy,
                                             minimum, readFile, replicateM,
                                             sequence_, stripPrefix, try,
                                             writeFile, ($>), (</>))
import           Control.DeepSeq            (NFData (..))
import           Control.Monad.Identity     (Identity (..))
import           Control.Monad.Except hiding (foldM)
import           Control.Monad.State.Strict (MonadState (..), StateT)
import           Data.Fix
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as H
import           Data.Sequence              (Seq)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Filesystem.Path.CurrentOS  hiding (concat, empty, null, (<.>))
import           GHC.Generics

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

instance Extract Identity where
  extract (Identity x) = x

-- | For things whose string representation needs to be computed with
-- potential side-effects.
class MonadIO io => ShowIO t io where
  showIO :: t -> io Text

instance ShowIO t io => ShowIO (io t) io where
  showIO action = action >>= showIO

instance (ShowIO a io, Traversable t, Show (t Text))
         => ShowIO (t (io a)) io where
  showIO vals = do
    innerReps <- mapM showIO vals
    return $ tshow innerReps

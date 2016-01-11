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
class ShowIO t where
  showIO :: t -> IO Text

instance ShowIO t => ShowIO (IO t) where
  showIO action = action >>= showIO

instance (ShowIO a, Traversable t, Show (t Text))
         => ShowIO (t (IO a)) where
  showIO vals = do
    innerReps <- mapM showIO vals
    return $ tshow innerReps

instance (Show a, ShowIO b) => ShowIO (Either a b) where
  showIO (Left a) = return $ tshow a
  showIO (Right b) = showIO b

instance (Show a, ShowIO b) => ShowIO (ExceptT a IO b) where
  showIO action = showIO =<< runExceptT action

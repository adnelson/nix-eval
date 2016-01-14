{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Nix.Values (
  module Nix.Values.Generic,
  module Nix.Values.Lazy,
  module Nix.Values.Strict
  ) where


import Nix.Values.Generic
import Nix.Values.Lazy
import Nix.Values.Strict

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Nix.Eval.Values (
  module Nix.Eval.Values.Generic,
  module Nix.Eval.Values.Lazy,
  module Nix.Eval.Values.Strict
  ) where


import Nix.Eval.Values.Generic
import Nix.Eval.Values.Lazy
import Nix.Eval.Values.Strict

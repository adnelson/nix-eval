{-# LANGUAGE NoImplicitPrelude #-}
module Nix.Eval.Constants where

import Nix.Common

data Constant
  = String Text | Int Int | Bool Bool | Null
  deriving (Show, Eq)

instance IsString Constant where
  fromString = String . fromString

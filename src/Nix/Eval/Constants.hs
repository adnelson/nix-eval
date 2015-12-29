{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Nix.Eval.Constants where

import Nix.Common

data Constant
  = String Text
  | Path FilePath
  | Int Integer
  | Bool Bool
  | Null
  deriving (Show, Eq)

instance IsString Constant where
  fromString = String . fromString

class FromConstant t where
  fromConstant :: Constant -> t

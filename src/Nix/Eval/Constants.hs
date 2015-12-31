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
  fromConstants :: [Constant] -> t
  fromConstantSet :: HashMap Text Constant -> t

fromInt :: (FromConstant t, Integral i) => i -> t
fromInt = fromConstant . Int . fromIntegral

fromInteg :: FromConstant t => Integer -> t
fromInteg = fromConstant . Int

fromText :: FromConstant t => Text -> t
fromText = fromConstant . String

fromBool :: FromConstant t => Bool -> t
fromBool = fromConstant . Bool

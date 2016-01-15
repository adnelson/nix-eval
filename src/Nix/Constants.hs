{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Nix.Constants where

import Nix.Common

data Constant
  = String Text
  | Path FilePath
  | Int Integer
  | Bool Bool
  | Null
  deriving (Show, Eq, Generic)

instance NFData Constant

instance IsString Constant where
  fromString = String . fromString

class ToConstant t where
  toConstant :: t -> Constant

instance ToConstant Text where toConstant = String
instance ToConstant Bool where toConstant = Bool
instance ToConstant FilePath where toConstant = Path
instance ToConstant Integer where toConstant = Int
instance ToConstant Int where toConstant = Int . fromIntegral

class FromConstant t where
  fromConstant :: Constant -> t
  fromConstants :: [Constant] -> t
  fromConstantSet :: HashMap Text Constant -> t

-- | Convert a primitive into something which can be made from a
-- constant. So for example `convert 1 :: Expression`
convert :: (ToConstant prim, FromConstant t) => prim -> t
convert = fromConstant . toConstant

fromInt :: (FromConstant t, Integral i) => i -> t
fromInt = fromConstant . Int . fromIntegral

fromInteg :: FromConstant t => Integer -> t
fromInteg = fromConstant . Int

fromText :: FromConstant t => Text -> t
fromText = fromConstant . String

fromBool :: FromConstant t => Bool -> t
fromBool = fromConstant . Bool

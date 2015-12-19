{-# LANGUAGE NoImplicitPrelude #-}
module Nix.Eval.RuntimeTypes where

import Nix.Common
import Nix.Eval.Expressions
import Nix.Eval.Values
import Nix.Eval.Constants

-- | Runtime types of values.
data RuntimeType
  = RT_Null
  | RT_Bool
  | RT_Int
  | RT_String
  | RT_Function
  | RT_List
  | RT_AttrSet
  deriving (Show, Eq, Ord)

typeOfConstant :: Constant -> RuntimeType
typeOfConstant (String _) = RT_String
typeOfConstant (Int _) = RT_Int
typeOfConstant (Bool _) = RT_Bool
typeOfConstant Null = RT_Null

typeOfValue :: Value -> RuntimeType
typeOfValue (VConstant constant) = typeOfConstant constant
typeOfValue (VAttrSet _) = RT_AttrSet
typeOfValue (VList _) = RT_List
typeOfValue (VFunction _ _) = RT_Function

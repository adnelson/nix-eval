{-# LANGUAGE NoImplicitPrelude #-}
module Nix.Eval.Expressions where

import Nix.Common
import Nix.Eval.Constants
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H

data BinOp
  = BO_Plus | BO_Minus | BO_Times | BO_Concat
  | BO_JoinSets | BO_And | BO_Or
  deriving (Show, Eq)

data Expression
  = EConstant Constant
  | EVar Text
  | EListLiteral [Expression]
  | EAttrSet (HashMap Text Expression)
  | EAttrReference Expression Text -- ^ Dots
  | EBinop BinOp
  | ELambda Text Expression
  | EApply Expression Expression
  | ELet (HashMap Text Expression) Expression
  deriving (Show, Eq)

constantE :: Constant -> Expression
constantE = EConstant

varE :: Text -> Expression
varE = EVar

letE :: HashMap Text Expression -> Expression -> Expression
letE bindings = ELet bindings

str :: Text -> Expression
str = EConstant . String

instance IsString Expression where
  fromString = EVar . fromString

{-# LANGUAGE NoImplicitPrelude #-}
module Nix.Eval.Expressions where

import Nix.Common
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H

data Constant
  = String Text | Int Int | Bool Bool | Null
  deriving (Show, Eq)

data BinOp
  = BO_Plus | BO_Minus | BO_Times | BO_Concat
  | BO_JoinSets | BO_And | BO_Or
  deriving (Show, Eq)

data Expression e
  = EConstant Constant
  | EVar Text
  | EListLiteral [e]
  | EAttrSet (HashMap Text e)
  | EBinop BinOp
  | ELambda Text e
  | EApply e e
  | ELet (HashMap Text e) e -- Might not be necessary
  deriving (Show, Eq)

newtype ExpressionF = ExpressionF (Expression ExpressionF)
  deriving (Show, Eq)

constantE :: Constant -> ExpressionF
constantE = ExpressionF . EConstant

varE :: Text -> ExpressionF
varE = ExpressionF . EVar

letE :: HashMap Text ExpressionF -> ExpressionF -> ExpressionF
letE bindings = ExpressionF . ELet bindings

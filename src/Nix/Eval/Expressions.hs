{-# LANGUAGE NoImplicitPrelude #-}
module Nix.Eval.Expressions where

import Nix.Common
import Nix.Eval.Constants
import qualified Data.HashMap.Strict as H

-- | Types of binary operations.
data BinaryOp
  = BO_Plus     -- ^ +
  | BO_Minus    -- ^ -
  | BO_Times    -- ^ *
  | BO_Concat   -- ^ ++
  | BO_JoinSets -- ^ //
  | BO_And      -- ^ &&
  | BO_Or       -- ^ ||
  deriving (Show, Eq)

-- | Types of unary operations.
data UnaryOp
  = UO_Not -- ^ !
  | UO_Neg -- ^ -
  deriving (Show, Eq)

-- | A simpler expression type than the full nix language expression; this
-- type can be seen as a desugared nix expression type.
data Expression
  = EConstant Constant
  -- ^ Constants such as numbers etc.
  | EVar Text
  -- ^ Variables.
  | EListLiteral [Expression]
  -- ^ List literals.
  | ENonRecursiveAttrs (HashMap Text Expression)
  -- ^ Attribute set literals, not recursive.
  | ERecursiveAttrs (HashMap Text Expression)
  -- ^ Attribute set literals, allowing recursive references.
  | EAttrReference Expression Text
  -- ^ Dot-references, like `a.b`
  | EBinaryOp Expression BinaryOp Expression
  -- ^ A binary operation on two expressions. We of course could implement
  -- these as curried functions.
  | EUnaryOp UnaryOp Expression
  -- ^ A unary operation; once again we could just use a function call.
  | ELambda Text Expression
  -- ^ Lambda functions.
  | EApply Expression Expression
  -- ^ Function application.
  | EWith Expression Expression
  -- ^ Expresses a `with` statement, but we also use it for `let` statements.
  -- The first expression is an expression which must evaluate to an
  -- attribute set. All of the keys in the attribute set will be added to
  -- the environment before evaluating the second expression.
  deriving (Show, Eq)

-- | A let statement of this form:
--
-- > let
-- >   x = 1;
-- >   y = x + 2;
-- > in z
--
-- Can be viewed as the following `with` statement:
--
-- > with rec {
-- >   x = 1;
-- >   y = x + 2;
-- > }; z
--
-- This function will produce this transition.
letE :: HashMap Text Expression -> Expression -> Expression
letE = EWith . ERecursiveAttrs

-- | Creates a string literal expression.
strE :: Text -> Expression
strE = constantE . String

-- | Creates an integer literal expression.
intE :: Integer -> Expression
intE = constantE . Int

-- | Creates a boolean literal expression.
boolE :: Bool -> Expression
boolE = constantE . Bool

-- | Turn a constant into an expression.
constantE :: Constant -> Expression
constantE = EConstant

-- | Turn a variable name into an expression.
varE :: Text -> Expression
varE = EVar

-- | Make an attribute set (non-recursive).
attrsE :: [(Text, Expression)] -> Expression
attrsE = ENonRecursiveAttrs . H.fromList

-- | Dot-reference into an attribute set.
(!.) :: Expression -> Text -> Expression
(!.) = EAttrReference

infixl 8 !.

-- | Function application expression.
($$) :: Expression -> Expression -> Expression
e $$ e' = EApply e e'

infixr 0 $$

-- | Lambda shorthand.
(-->) :: Text -> Expression -> Expression
param --> body = ELambda param body

infixr 1 -->

-- | We'll make Expressions's string instance be variables, rather than
-- string literal expressions.
instance IsString Expression where
  fromString = EVar . fromString

-- | Expressions can be parsed from numbers.
instance Num Expression where
  fromInteger = intE
  e1 + e2 = EBinaryOp e1 BO_Plus e2
  e1 - e2 = EBinaryOp e1 BO_Minus e2
  e1 * e2 = EBinaryOp e1 BO_Times e2
  negate = EUnaryOp UO_Neg
  abs = error "No absolute value for Nix expressions"
  signum = error "No sign for Nix expressions"

-- | Wrapper for binary `and`
andE :: Expression -> Expression -> Expression
andE e1 e2 = EBinaryOp e1 BO_And e2

orE :: Expression -> Expression -> Expression
orE e1 e2 = EBinaryOp e1 BO_Or e2

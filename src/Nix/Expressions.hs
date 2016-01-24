module Nix.Expressions where

import Nix.Common
import Nix.Expr (NBinaryOp(..), NUnaryOp(..), Params(..), ParamSet(..))
import Nix.Constants
import qualified Data.HashMap.Strict as H
import qualified Data.Map as M
import qualified Data.Set as S

-- | A simpler expression type than the full nix language expression; this
-- type can be seen as a desugared nix expression type.
data Expression
  = EConstant Constant
  -- ^ Constants such as numbers etc.
  | EVar Text
  -- ^ Variables.
  | EList (Seq Expression)
  -- ^ List literals.
  | ENonRecursiveAttrs (Record Expression)
  -- ^ Attribute set literals, not recursive.
  | ERecursiveAttrs (Record Expression)
  -- ^ Attribute set literals, allowing recursive references.
  | EAttrReference Expression Text
  -- ^ Dot-references, like `a.b`
  | EBinaryOp Expression NBinaryOp Expression
  -- ^ A binary operation on two expressions. We of course could implement
  -- these as curried functions.
  | EUnaryOp NUnaryOp Expression
  -- ^ A unary operation; once again we could just use a function call.
  | ELambda (Params Expression) Expression
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
letsE :: [(Text, Expression)] -> Expression -> Expression
letsE = EWith . ERecursiveAttrs . H.fromList

-- | Wrapper for a single-variable @let@.
letE :: Text -> Expression -> Expression -> Expression
letE varName varExpr = letsE [(varName, varExpr)]

-- | Creates a string literal expression.
strE :: Text -> Expression
strE = fromConstant . String

-- | Creates an integer literal expression.
intE :: Integer -> Expression
intE = fromConstant . Int

-- | Creates a boolean literal expression.
boolE :: Bool -> Expression
boolE = fromConstant . Bool

-- | Creates a null literal expression.
nullE :: Expression
nullE = fromConstant Null

instance FromConstant Expression where
  fromConstant = EConstant
  fromConstants = listE . map fromConstant
  fromConstantSet = ENonRecursiveAttrs . map fromConstant

-- | Turn a variable name into an expression.
varE :: Text -> Expression
varE = EVar

-- | Make a @with@ expression.
withE :: Expression -> Expression -> Expression
withE = EWith

-- | Make an attribute set (non-recursive).
attrsE :: [(Text, Expression)] -> Expression
attrsE = ENonRecursiveAttrs . H.fromList

-- | Make an attribute set (recursive).
recAttrsE :: [(Text, Expression)] -> Expression
recAttrsE = ERecursiveAttrs . H.fromList

-- | Make a list.
listE :: [Expression] -> Expression
listE = EList . fromList

-- | Logical negation.
notE :: Expression -> Expression
notE = EUnaryOp NNot

-- | Dot-reference into an attribute set.
(!.) :: Expression -> Text -> Expression
(!.) = EAttrReference
infixl 8 !.

-- | Various nix binary operators
($==), ($!=), ($<), ($<=), ($>), ($>=), ($&&), ($||), ($->),
  ($//), ($+), ($-), ($*), ($/), ($++)
  :: Expression -> Expression -> Expression
e1 $== e2 = EBinaryOp e1 NEq e2
e1 $!= e2 = EBinaryOp e1 NNEq e2
e1 $< e2 = EBinaryOp e1 NLt e2
e1 $<= e2 = EBinaryOp e1 NLte e2
e1 $> e2 = EBinaryOp e1 NGt e2
e1 $>= e2 = EBinaryOp e1 NGte e2
e1 $&& e2 = EBinaryOp e1 NAnd e2
e1 $|| e2 = EBinaryOp e1 NOr e2
e1 $-> e2 = EBinaryOp e1 NImpl e2
e1 $// e2 = EBinaryOp e1 NUpdate e2
e1 $+ e2 = EBinaryOp e1 NPlus e2
e1 $- e2 = EBinaryOp e1 NMinus e2
e1 $* e2 = EBinaryOp e1 NMult e2
e1 $/ e2 = EBinaryOp e1 NDiv e2
e1 $++ e2 = EBinaryOp e1 NConcat e2

-- | Function application expression.
(@@) :: Expression -> Expression -> Expression
e @@ e' = EApply e e'
infixl 1 @@

-- | Lambda shorthand.
(-->) :: Text -> Expression -> Expression
param --> body = ELambda (Param param) body

infixr 1 -->

paramList :: ParamSet e -> [(Text, Maybe e)]
paramList (FixedParamSet params) = M.toList params
paramList (VariadicParamSet params) = M.toList params

-- | We'll make Expressions's string instance be variables, rather than
-- string literal expressions.
instance IsString Expression where
  fromString = EVar . fromString

-- | Expressions can be parsed from numbers.
instance Num Expression where
  fromInteger = intE
  e1@(EList _) + e2 = EBinaryOp e1 NConcat e2
  e1 + e2 = EBinaryOp e1 NPlus e2
  e1 - e2 = EBinaryOp e1 NMinus e2
  e1 * e2 = EBinaryOp e1 NMult e2
  negate = EUnaryOp NNeg
  abs = error "No absolute value for Nix expressions"
  signum = error "No sign for Nix expressions"

-- | Describes the types of objects that can be encountered at runtime.
module Nix.Evaluator.RuntimeTypes where

import Nix.Common
import Nix.Atoms
import qualified Data.Text as T

-- | Runtime types of values.
data RuntimeType
  = RT_Null
  | RT_Bool
  | RT_Int
  | RT_String
  | RT_Lambda
  | RT_List
  | RT_Set
  | RT_Path
  deriving (Show, Eq, Ord, Enum, Generic)

instance NFData RuntimeType

-- | String representation of runtime types.
typeToString :: RuntimeType -> Text
typeToString = T.toLower . T.replace "RT_" "" . tshow

-- | Atoms have a runtime type which can be determined in O(1).
typeOfAtom :: NAtom -> RuntimeType
typeOfAtom (NUri _) = RT_String
typeOfAtom (NInt _) = RT_Int
typeOfAtom (NBool _) = RT_Bool
typeOfAtom NNull = RT_Null

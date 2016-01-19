-- | Describes the types of objects that can be encountered at runtime.
module Nix.Eval.RuntimeTypes where

import Nix.Common
import Nix.Constants
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

-- | Constants have a runtime type which can be determined in O(1).
typeOfConstant :: Constant -> RuntimeType
typeOfConstant (String _) = RT_String
typeOfConstant (Path _) = RT_Path
typeOfConstant (Int _) = RT_Int
typeOfConstant (Bool _) = RT_Bool
typeOfConstant Null = RT_Null

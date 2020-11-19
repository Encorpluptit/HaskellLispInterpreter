module HalDataTypes where

import qualified Data.Map as Map
import HalError
import LispExpression

type Env = Map.Map String HalExpr

emptyEnv :: Env
emptyEnv = Map.empty
--addVarToEnv ::

newtype Built = Built ([HalExpr] -> ThrowsHalExprError HalExpr)

type ThrowsHalExprError = ThrowsError HalExpr

instance Show Built where
  show _ = "#<procedure>"

data HalExpr
  = Value LispExpr
  | Bool Bool
  | Builtin Built
  | Lambda (Maybe String) [String] HalExpr Env
  | FunCall HalExpr [HalExpr]
  | If HalExpr HalExpr HalExpr
  | Ref String
  | Def String HalExpr
  deriving (Show)

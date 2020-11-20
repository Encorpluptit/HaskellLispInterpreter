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

unpackHalExprError :: ThrowsHalExprError b -> Either (HALError HalExpr) b
unpackHalExprError (HandleError val) = val

instance Show Built where
  show _ = "#<procedure>"

data HalExpr
  = Value LispExpr
  | Bool Bool
  | Builtin Built
  deriving (Show)

showHalExpr :: HalExpr -> String
showHalExpr _ = "TO IMPLEMENT"
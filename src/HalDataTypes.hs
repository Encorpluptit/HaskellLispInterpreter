module HalDataTypes where

import qualified Data.Map as Map
import HalError
import LispExpression

type Identifier = String

type Env = Map.Map String HalExpr


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

printHalExpr :: Monad m => Bool -> (String -> m ()) -> HalExpr -> m ()
printHalExpr False f = f . showHalExpr
printHalExpr True f = f . show

showHalExpr :: HalExpr -> String
showHalExpr (Value val) = show val
showHalExpr (Bool False) = "#f"
showHalExpr (Bool True) = "#t"
showHalExpr _ = "TO IMPLEMENT"
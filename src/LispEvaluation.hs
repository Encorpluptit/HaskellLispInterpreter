module LispEvaluation where

import LispExpression
import HalDataTypes
import HalError
import HalOptions

import Debug.Trace


evalLispExprList :: Env -> [LispExpr] -> ThrowsHalExprError (Env, Maybe HalExpr)
evalLispExprList env [] = return (env, Nothing)
evalLispExprList env [x] = evalLispExpr env x
evalLispExprList env (x : xs) = evalLispExpr env x >>= \(newEnv, _) -> evalLispExprList newEnv xs

evalLispExpr :: Env -> LispExpr -> ThrowsHalExprError (Env, Maybe HalExpr)
--evalLispExpr _ _ = throw $ UnknownError "LOL ICI"
evalLispExpr env l = trace (show l) return (env, Just $ Bool True )

module LispEvaluation where

import LispExpression
import HalDataTypes
import HalError
import HalOptions
import HalEvaluation

import Debug.Trace


--evalLispExpr :: String -> Env -> ThrowsHalExprError (Env, Maybe HalExpr)
--evalLispExpr file env = compAll <$> readFile file
--  where compAll str = parseContent str >>= evalEnvAll env
--
evalLispExprList :: [LispExpr] -> Env -> ThrowsHalExprError (Env, Maybe HalExpr)
evalLispExprList [] env = return (env, Nothing)
evalLispExprList [x] env = evalLispExpr env x
evalLispExprList (x : xs) env =
    evalLispExpr env x >>= evalLispExprList xs . fst

evalLispExpr :: Env -> LispExpr -> ThrowsHalExprError (Env, Maybe HalExpr)
evalLispExpr env expr = evalLispValue env expr >>= eval
--    where eval halExpr =  evalHalExpr env halExpr >>= \res -> return (env, Just res)
    where eval halExpr =  (,) env . Just <$> evalHalExpr env halExpr
--    where eval halExpr = do
--            res <- evalHalExpr env halExpr
--            return (env, Just res)

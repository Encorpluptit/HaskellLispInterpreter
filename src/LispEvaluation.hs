module LispEvaluation where

import qualified Data.Map as Map
import LispExpression
import HalDataTypes
import HalEvaluation
import HalError


--evalLispExpr :: String -> Env -> ThrowsHalExprError (Env, Maybe HalExpr)
--evalLispExpr file env = compAll <$> readFile file
--  where compAll str = parseContent str >>= evalEnvAll env
--
evalLispExprList :: Env -> [LispExpr] -> ThrowsHalExprError (Env, Maybe HalExpr)
evalLispExprList env [] = return (env, Nothing)
evalLispExprList env [x] = evalLispExpr env x
evalLispExprList env (x : xs) = evalLispExpr env x >>= \(newEnv, _) -> evalLispExprList newEnv xs
--evalEnvAll env (x : xs) = do
--  (env2, res) <- evalEnv env x
--  evalEnvAll env2 xs

evalLispExpr :: Env -> LispExpr -> ThrowsHalExprError (Env, Maybe HalExpr)
evalLispExpr _ _ = throw $ UnknownError "LOL ICI"
--evalLispExpr :: Env -> LispExpr -> ThrowsHalExprError (Env, Maybe HalExpr)
--evalLispExpr env x = compile env x >>= evalEnv'
--  where
--    evalEnv' (Def name expr) = do
--      e2 <- eval env expr
--      return (Map.insert name e2 env, Nothing)
--    evalEnv' e1 = do
--      res <- eval env e1
--      return (env, Just res)

--evalEnvAll :: Env -> [LispExpr] -> ThrowsHalExprError (Env, Maybe HalExpr)
--evalEnvAll env [] = return (env, Nothing)
--evalEnvAll env [x] = evalEnv env x
--evalEnvAll env (x : xs) = do
--  (env2, res) <- evalEnv env x
--  evalEnvAll env2 xs
--
--evalEnv :: Env -> LispExpr -> ThrowsHalExprError (Env, Maybe HalExpr)
--evalEnv env x = compile env x >>= evalEnv'
--  where
--    evalEnv' (Def name expr) = do
--      e2 <- eval env expr
--      return (Map.insert name e2 env, Nothing)
--    evalEnv' e1 = do
--      res <- eval env e1
--      return (env, Just res)

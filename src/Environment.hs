module Environment where

import qualified Data.Map as Map
import DataTypes
import Errors


newtype Env = Env (Map.Map Identifier LispVal)
--    deriving (Show)

instance Show Env where
--    show (Env env) = "Env:" ++ foldl1 (++) (map (("\n\t" ++) . show)(Map.keys env))
    show (Env env) = "Env: " ++ envMap
        where
            keys = Map.keys env
            envMap
                | not (null keys) = foldl1 (++) (map (("\n\t" ++) . show) (Map.keys env))
                | otherwise = "[Empty]"

emptyEnv :: Env
emptyEnv = Env Map.empty

getEnvVar :: Env -> Identifier -> ThrowsError LispVal
getEnvVar (Env env) ident = case Map.lookup ident env of
    Nothing -> throw $ UnboundVar ident
    Just a -> return a

getEnvVar' :: Env -> Identifier -> ThrowsError (LispVal, Env)
getEnvVar' (Env env) ident = case Map.lookup ident env of
    Nothing -> throw $ UnboundVar ident
    Just a -> return (a, Env env)

addEnvVar :: Identifier -> LispVal -> Env -> Env
addEnvVar ident val (Env env) = Env $ Map.insert ident val env

--getEnvVar' :: Env -> Identifier -> ThrowsError (LispVal, Env)



toMap :: Env -> Map.Map Identifier LispVal
toMap (Env m) = m

--type ValCtx = Map.Map Identifier LispVal
--
--type FuncCtx = Map.Map Identifier LispVal
--
--data EnvCtx = EnvCtx
--  { env :: ValCtx,
--    funcenv :: FuncCtx
--  }
--  deriving (Eq)
--
--emptyEnvCtx :: EnvCtx
--emptyEnvCtx = EnvCtx {env = Map.empty, funcenv = Map.empty}

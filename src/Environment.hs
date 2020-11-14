module Environment where

--newtype Env = Env (Map.Map Identifier LispVal)
----    deriving (Show)
--
--instance Show Env where
----    show (Env env) = "Env:" ++ foldl1 (++) (map (("\n\t" ++) . show)(Map.keys env))
----    show (Env env) = "Env: " ++ envMap
----        where
----            keys = Map.keys env
----            envMap
----                | not (null keys) = foldl1 (++) (map (("\n\t" ++) . show) (Map.keys env))
----                | otherwise = "[Empty]"
--    show (Env env) = "Env: " ++ envMap
--        where
--            keys = Map.keys env
--            envMap
--                | not (null keys) = foldl1 (++) (map (("\n\t" ++) . show) (Map.toList env))
--                | otherwise = "[Empty]"

--emptyEnv :: Env
--emptyEnv = Env Map.empty
--
--getEnvVar :: Env -> Identifier -> ThrowsError LispVal
--getEnvVar (Env env) ident = case Map.lookup ident env of
--    Nothing -> throw $ UnboundVar ident
--    Just a -> return a
--
--getEnvVar' :: Env -> Identifier -> ThrowsError (LispVal, Env)
--getEnvVar' (Env env) ident = case Map.lookup ident env of
--    Nothing -> throw $ UnboundVar ident
--    Just a -> return (a, Env env)
--
--addEnvVar :: Identifier -> LispVal -> Env -> Env
--addEnvVar ident val (Env env) = Env $ Map.insert ident val env

--getEnvVar' :: Env -> Identifier -> ThrowsError (LispVal, Env)

--toMap :: Env -> Map.Map Identifier LispVal
--toMap (Env m) = m

import qualified Data.Map as Map
import Builtins
import DataTypes
import Errors

newtype EnvVar = EnvVar (Map.Map Identifier LispVal)
  deriving (Show)

newtype EnvFunc = EnvFunc (Map.Map Identifier ([LispVal] -> ThrowsError LispVal))

instance Show EnvFunc where
  show (EnvFunc env) = show (Map.keys env)

data Env = Env
  { varsEnv :: EnvVar,
    funcEnv :: EnvFunc
  }
  deriving (Show)

emptyEnv :: Env
emptyEnv = Env {varsEnv = EnvVar Map.empty, funcEnv = EnvFunc $ Map.fromList builtins}

getSubEnv :: Env -> [(Identifier, LispVal)] -> Env
getSubEnv (Env (EnvVar env) fenv) newEnv =
  Env
    { varsEnv = EnvVar $ Map.union (Map.fromList newEnv) env,
      funcEnv = fenv
    }

addEnvVar :: Env -> Identifier -> LispVal -> Env
addEnvVar (Env (EnvVar env) fenv) ident val =
  Env
    { varsEnv = EnvVar $ Map.insert ident val env,
      funcEnv = fenv
    }

getEnvVar :: Env -> Identifier -> ThrowsError (LispVal, Env)
getEnvVar globalEnv@(Env (EnvVar env) _) ident = case Map.lookup ident env of
  Nothing -> throw $ UnboundVar ident
  Just a -> return (a, globalEnv)

addEnvFunc :: Env -> Identifier -> ([LispVal] -> ThrowsError LispVal) -> Env
addEnvFunc (Env varenv (EnvFunc env)) ident val =
  Env
    { varsEnv = varenv,
      funcEnv = EnvFunc $ Map.insert ident val env
    }

getEnvFunc :: Env -> Identifier -> ThrowsError ([LispVal] -> ThrowsError LispVal)
getEnvFunc (Env _ (EnvFunc env)) ident = case Map.lookup ident env of
  Nothing -> throw $ UnboundVar ident
  Just a -> return a

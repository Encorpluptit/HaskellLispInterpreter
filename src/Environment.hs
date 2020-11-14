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

newtype ValCtx = ValCtx (Map.Map Identifier LispVal)
  deriving (Show)

newtype FuncCtx = FuncCtx (Map.Map Identifier ([LispVal] -> ThrowsError LispVal))

instance Show FuncCtx where
  show (FuncCtx env) = show (Map.keys env)

data Env = Env
  { varsEnv :: ValCtx,
    funcEnv :: FuncCtx
  }
  deriving (Show)

emptyEnv :: Env
emptyEnv = Env {varsEnv = ValCtx Map.empty, funcEnv = FuncCtx $ Map.fromList builtins}

getSubEnv :: Env -> [(Identifier, LispVal)] -> Env
getSubEnv (Env (ValCtx env) fenv) newEnv =
  Env
    { varsEnv = ValCtx $ Map.union (Map.fromList newEnv) env,
      funcEnv = fenv
    }

addEnvVar :: Env -> Identifier -> LispVal -> Env
addEnvVar (Env (ValCtx env) fenv) ident val =
  Env
    { varsEnv = ValCtx $ Map.insert ident val env,
      funcEnv = fenv
    }

getEnvVar :: Env -> Identifier -> ThrowsError (LispVal, Env)
getEnvVar globalEnv@(Env (ValCtx env) _) ident = case Map.lookup ident env of
  Nothing -> throw $ UnboundVar ident
  Just a -> return (a, globalEnv)

addEnvFunc :: Env -> Identifier -> ([LispVal] -> ThrowsError LispVal) -> Env
addEnvFunc (Env varenv (FuncCtx env)) ident val =
  Env
    { varsEnv = varenv,
      funcEnv = FuncCtx $ Map.insert ident val env
    }

getEnvFunc :: Env -> Identifier -> ThrowsError ([LispVal] -> ThrowsError LispVal)
getEnvFunc (Env _ (FuncCtx env)) ident = case Map.lookup ident env of
  Nothing -> throw $ UnboundVar ident
  Just a -> return a

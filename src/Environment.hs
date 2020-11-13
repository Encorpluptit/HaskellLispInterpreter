module Environment where

import qualified Data.Map as Map
import DataTypes
import Errors

type Identifier = String

newtype Env = Env (Map.Map Identifier ([LispVal] -> ThrowsError LispVal))
--    deriving (Show)

instance Show Env where
    show (Env env) = "Env:" ++ foldl1 (++) (map (("\n\t" ++) . show) (Map.keys env))

emptyEnv :: Env
emptyEnv = Env Map.empty

toMap :: Env -> Map.Map Identifier ([LispVal] -> ThrowsError LispVal)
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

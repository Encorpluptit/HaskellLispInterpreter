module Environment where

import DataTypes
import Errors
import qualified Data.Map as Map

type Identifier = String

newtype Env = Env (Map.Map Identifier LispVal)

emptyEnv :: Env
emptyEnv = Env Map.empty

type ValCtx = Map.Map Identifier LispVal
type FuncCtx  = Map.Map Identifier LispVal

data EnvCtx = EnvCtx
  {
    env :: ValCtx
  , funcenv :: FuncCtx
  } deriving (Eq)

emptyEnvCtx :: EnvCtx
emptyEnvCtx = EnvCtx {env = Map.empty, funcenv = Map.empty}

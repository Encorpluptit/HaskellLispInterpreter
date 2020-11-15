module DataTypes
  ( LispVal (..),
    showVal,
    Identifier,
    Env(..),
    HALError(..),
    ThrowsError(..),
    LispFct(..),
    throw,
    unpackError,
    addEnvFuncList,
    emptyEnv,
    addVarsToEnv,
    addEnvVar,
    getEnvVar,
    addEnvFunc,
    getEnvFunc,
    mergeEnvs
  )
where

import Control.Applicative
import qualified Data.Map as Map
--import HalError
--import HalErrorsMonad
--import Data.Ratio

type Identifier = String

--import qualified Data.Map as Map
----newtype Env = Env (Map.Map String LispVal)
--type Identifier = String
--newtype Env = Env (Map.Map Identifier ([LispVal] -> LispVal))
--
--emptyEnv :: Env
--emptyEnv = Env Map.empty

--data LispNum = NumInt Int
--            | NumInteger Integer
--            -- Keep which one ?
--            | NumFloat Float
--            | NumDouble Double
--            --
--            | NumRational Rational
----            | NumFract (Fractional)
--                deriving (Show)
--
--data LispVal = ValNumber LispNum
--            | ValString String
--            | ValBool Bool
--                deriving (Show)

--data LispData = Value LispVal
--             | List [LispData]
--             | DottedList [LispData] LispData
--                deriving (Show)
----             | Func (LispData -> ThrowsError LispData)

data HALError
  = UnknownError String -- generic Error
  | ParsingError LispVal String -- (eq? 1)
  | NbArgsError String Integer [LispVal] -- (eq? 1)
  | TypeError String LispVal -- (eq? 1 "l")
  | UnboundVar String -- (eq? foo 1)   { foo not defined }
  | BuiltinError String [LispVal]
  | NotFunction String LispVal         -- (foo 1)       { (define foo 0) }
  | KeywordError LispVal -- Useful with unbound Var ?
  | SyntaxError String -- (define foo 0
--    deriving (Eq)
  -- | FileError     String          -- file don't exist ? Wrong Syntax in file ?

instance Show HALError where
  show = showHALError

showHALError :: HALError -> String
showHALError (UnknownError msg) = "Unknown Error: " ++ msg
showHALError (ParsingError parsed left) = "Parsing Error: Parsed:\n" ++ show parsed ++ "\nLeft: "++ show left
showHALError (TypeError msg val) = "Wrong type: " ++ msg ++ " -> " ++ show val
showHALError (NbArgsError op nb val) =
  "Wrong Number of Args in operator { " ++ op ++ " }. Expected "
    ++ show nb
    ++ " operand(s) -> Got: " ++ show (length val) ++ " -> "
    ++ show val
showHALError (UnboundVar var) = "UnboundVar: " ++ var
showHALError (BuiltinError builtin args) = "Unrecognised " ++ builtin ++ " (built-in) args: " ++ show args
showHALError (SyntaxError msg) = msg
showHALError (KeywordError val) = "KeyWord Error, got : " ++ show val
showHALError (NotFunction msg val) = msg ++ " : " ++ show val


newtype ThrowsError a = HandleError (Either HALError a)
--  deriving (Show, Eq)
  deriving (Show)

instance Functor ThrowsError where
  fmap _ (HandleError (Left err)) = HandleError $ Left err
  fmap f (HandleError (Right val)) = HandleError . Right $ f val

instance Applicative ThrowsError where
  pure = HandleError . Right
  (HandleError (Left err)) <*> _ = HandleError (Left err)
  (HandleError (Right f)) <*> x = fmap f x

instance Alternative ThrowsError where
  empty = HandleError . Left $ UnknownError "parser Empty"
  (HandleError a) <|> (HandleError b) = case a of
    Right val -> HandleError (Right val)
    Left err -> case b of
         Right val -> HandleError (Right val)
         Left _ -> HandleError (Left err)

instance Monad ThrowsError where
  (HandleError te) >>= f =
    case te of
      Left err -> HandleError (Left err)
      Right val -> f val

  return val = HandleError $ Right val

throw :: HALError -> ThrowsError a
throw err = HandleError $ Left err

unpackError :: ThrowsError a -> Either HALError a
unpackError (HandleError val) = val

newtype LispFct = LispFct (Env -> [LispVal] -> ThrowsError LispVal)

instance Show LispFct where
    show _ = ""

data LispVal
  = Atom String
  | ValList [LispVal]
  | ValDottedList [LispVal] LispVal
  | ValNum Integer
  | ValBool Bool
  | ValString String
  -- | Func (LispVal -> ThrowsError LispVal)
  -- | Func ([LispVal] -> ThrowsError LispVal))
--  | Func Env (Env -> [LispVal] -> ThrowsError LispVal)
  | Func Env LispFct
  -- | Lambda IFunc EnvCtx
  deriving (Show)

-- | -----------------------------------------------------------------------------------------------------------------
-- TODO: Double, rational, etc. implementation via LispNum ?
-- | -----------------------------------------------------------------------------------------------------------------
-- TODO: Char implementation
--  * Char (WARNING: To parse before Atom):
--      - https://schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.3.4
--            | ValChar Char
-- | -----------------------------------------------------------------------------------------------------------------
--                deriving (Eq)

--
--instance Show [LispVal] where
--    show val = map showVal val

showVal :: LispVal -> String
showVal val =
  case val of
    (Atom atom) -> atom
    (ValString txt) -> "\"" ++ txt ++ "\""
    (ValNum num) -> show num
    (ValBool True) -> "#t"
    (ValBool False) -> "#f"
    --    Nil                 -> "'()"
    (ValList []) -> "'()"
    (ValDottedList vals end) -> "(" ++ unwordsListVal vals ++ " . " ++ showVal end ++ ")"
    (ValList vals) -> "(" ++ unwordsListVal vals ++ ")"
    (Func _ _) -> "#<procedure>"

--    (Fun _ )        -> "(internal function)"
--    (Lambda _ _)    -> "(lambda function)"

unwordsListVal :: [LispVal] -> String
unwordsListVal list = unwords $ showVal <$> list

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
emptyEnv = Env {varsEnv = EnvVar Map.empty, funcEnv = EnvFunc Map.empty}

addVarsToEnv :: Env -> [(Identifier, LispVal)] -> Env
addVarsToEnv (Env (EnvVar env) fenv) newEnv =
  Env
    { varsEnv = EnvVar $ Map.union (Map.fromList newEnv) env,
      funcEnv = fenv
    }

mergeEnvs :: Env -> Env -> Env
mergeEnvs (Env (EnvVar envVar1) (EnvFunc envFunc1)) (Env (EnvVar envVar2) (EnvFunc envFunc2)) =
    Env
      { varsEnv = EnvVar $ Map.union envVar1 envVar2,
        funcEnv = EnvFunc $ Map.union envFunc1 envFunc2
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

addEnvFuncList :: Env -> [(String, [LispVal] -> ThrowsError LispVal)] -> Env
addEnvFuncList (Env varenv (EnvFunc env)) list =
  Env
    { varsEnv = varenv,
      funcEnv = EnvFunc $ Map.union env (Map.fromList list)
    }

getEnvFunc :: Env -> Identifier -> ThrowsError ([LispVal] -> ThrowsError LispVal)
getEnvFunc (Env _ (EnvFunc env)) ident = case Map.lookup ident env of
  Nothing -> throw $ UnboundVar ident
  Just a -> return a

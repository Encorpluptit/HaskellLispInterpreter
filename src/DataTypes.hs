module DataTypes
  ( LispVal (..),
    Identifier,
    Env (..),
    HALError (..),
    ThrowsError (..),
    LispFct (..),
    showVal,
    throw,
    unpackError,
    emptyEnv,
    addVarsToEnv,
    addEnvVar,
    getEnvVar,
    getEnvVar',
    mergeEnvs,
  )
where

import Control.Applicative
import qualified Data.Map as Map

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
  = UnknownError String
  | ParsingError LispVal String
  | NbArgsError String Integer [LispVal]
  | TypeError String LispVal
  | UnboundVar Identifier
  | BuiltinError String [LispVal]
  | NotFunction LispVal
  | KeywordError LispVal
  | SyntaxError String
  deriving (Eq)

--    deriving (Eq)

-- | FileError     String          -- file don't exist ? Wrong Syntax in file ?
instance Show HALError where
  show = showHALError

showHALError :: HALError -> String
showHALError (UnknownError msg) = "Unknown Error: " ++ msg
showHALError (ParsingError parsed left) = "Parsing Error: Parsed:\n" ++ show parsed ++ "\nLeft: " ++ show left
showHALError (TypeError msg val) = "Wrong type: " ++ msg ++ " -> " ++ show val
showHALError (NbArgsError op nb val) =
  "Wrong Number of Args in operator { " ++ op ++ " }. Expected "
    ++ show nb
    ++ " operand(s) -> Got: "
    ++ show (length val)
    ++ " -> "
    ++ show val
showHALError (UnboundVar var) = var ++ " : not defined"
showHALError (BuiltinError builtin args) = "Unrecognised " ++ builtin ++ " (built-in) args: " ++ show args
showHALError (SyntaxError msg) = msg
showHALError (KeywordError val) = "KeyWord Error, got : " ++ show val
showHALError (NotFunction val) = "attempt to apply non-procedure #t" ++ " : " ++ show val

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
  show _ = "#<procedure>"

instance Eq LispFct where
  _ == _ = False

data LispVal
  = Atom String
  | ValList [LispVal]
  | ValDottedList [LispVal] LispVal
  | ValNum Integer
  | ValBool Bool
  | ValString String
  | Func Env LispFct
  deriving (Eq)

--  deriving (Show)

instance Show LispVal where
  show (Atom name) = "Atom \"" ++ name ++ "\""
  show (ValNum num) = "ValNum " ++ show num
  show (ValList list) = "ValList [" ++ foldl1 ((++) . (++ ", ")) (show <$> list) ++ "]"
  show (ValDottedList list left) = "ValDottedList (" ++ show list ++ " . " ++ show left ++ ")"
  show (ValBool bool) = "ValNum " ++ show bool
  show (ValString str) = "ValString \"" ++ str ++ "\""
  show (Func _ func) = "Func {Intern Env} " ++ show func

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
    (ValList []) -> "'()"
    (ValDottedList vals end) -> "(" ++ unwordsListVal vals ++ " . " ++ showVal end ++ ")"
    (ValList vals) -> "(" ++ unwordsListVal vals ++ ")"
    (Func _ _) -> "#<procedure>"

unwordsListVal :: [LispVal] -> String
unwordsListVal list = unwords $ showVal <$> list

newtype EnvVar = EnvVar (Map.Map Identifier LispVal)
  deriving (Show)

newtype Env = Env (Map.Map Identifier LispVal)
  deriving (Eq)

--instance Show Env where
--  show (Env env) = "Env : " ++ show (Map.keys env)

instance Show Env where
  show (Env env) = "Env: " ++ envMap
    where
      emptyenv = (not . null . Map.keys) env
      envMap
        | emptyenv = foldl1 (++) (map (("\n\t" ++) . show) (Map.toList env))
        | otherwise = "[Empty]"

emptyEnv :: Env
emptyEnv = Env Map.empty

addVarsToEnv :: Env -> [(Identifier, LispVal)] -> Env
addVarsToEnv (Env env) newEnv = Env $ Map.union (Map.fromList newEnv) env

mergeEnvs :: Env -> Env -> Env
mergeEnvs (Env env1) (Env env2) = Env $ Map.union env1 env2

addEnvVar :: Env -> Identifier -> LispVal -> Env
addEnvVar (Env env) ident val = Env $ Map.insert ident val env

getEnvVar :: Env -> Identifier -> ThrowsError (LispVal, Env)
getEnvVar globalEnv@(Env env) ident = case Map.lookup ident env of
  Nothing -> throw $ UnboundVar ident
  Just a -> return (a, globalEnv)

getEnvVar' :: Env -> Identifier -> ThrowsError LispVal
getEnvVar' (Env env) ident = case Map.lookup ident env of
  Nothing -> throw $ UnboundVar ident
  Just a -> return a

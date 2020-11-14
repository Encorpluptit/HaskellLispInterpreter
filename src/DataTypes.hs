module DataTypes
  ( LispVal (..),
    Identifier,
    showVal,
  )
where

--import Errors
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

data LispVal
  = Atom String
  | ValList [LispVal]
  | ValDottedList [LispVal] LispVal
  | ValNum Integer
  | ValBool Bool
  | ValString String
  -- | Func (LispVal -> ThrowsError LispVal)
  -- | Func ([LispVal] -> ThrowsError LispVal))
  -- | Func Env (Env -> [LispData] -> ThrowsError LispData)
  -- | Lambda IFunc EnvCtx
  deriving (Eq, Show)

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

--    (Fun _ )        -> "(internal function)"
--    (Lambda _ _)    -> "(lambda function)"

unwordsListVal :: [LispVal] -> String
unwordsListVal list = unwords $ showVal <$> list

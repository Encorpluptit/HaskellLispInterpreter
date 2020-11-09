module DataTypes
(
    LispVal(..),
    showVal
) where

--import Errors
--import Data.Ratio

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

data LispVal = Atom String
            | ValList [LispVal]
-- | -----------------------------------------------------------------------------------------------------------------
-- TODO: Double, rational, etc. implementation via LispNum ?
            | ValNum Integer
-- | -----------------------------------------------------------------------------------------------------------------
            | ValBool Bool
            | ValString String
--            | Fun IFunc
--            | Lambda IFunc EnvCtx
--          TODO: Keep Nil ? Because lot easier for visibility in pattern matching but bad for internal usage to differentiate
--          Solution: Alias ?
--            | Nil
-- | -----------------------------------------------------------------------------------------------------------------
-- TODO: Char implementation
--  * Char (WARNING: To parse before Atom):
--      - https://schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.3.4
--            | ValChar Char
-- | -----------------------------------------------------------------------------------------------------------------
--                deriving (Eq)
                deriving (Eq, Show)
--
--instance Show LispVal where
--    show val = showVal val

showVal :: LispVal -> String
showVal val =
  case val of
    (Atom atom)         -> atom
    (ValString txt)     -> "\"" ++ txt ++ "\""
    (ValNum num)        -> show num
    (ValBool True)      -> "#t"
    (ValBool False)     -> "#f"
--    Nil                 -> "'()"
    (ValList [])        -> "'()"
    (ValList vals)  -> "(" ++ unwordsListVal vals ++  ")"
--    (Fun _ )        -> "(internal function)"
--    (Lambda _ _)    -> "(lambda function)"

unwordsListVal :: [LispVal] -> String
unwordsListVal list = unwords $ showVal <$> list


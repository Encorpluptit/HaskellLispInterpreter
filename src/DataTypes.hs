module DataTypes
(
    LispVal(..)
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
            -- To replace by  LispNum ??
            | ValNum Integer
            --
            | ValBool Bool
            | ValString String
--            | Fun IFunc
--            | Lambda IFunc EnvCtx
            | Nil
--                deriving (Eq)
                deriving (Eq, Show)

--instance Show LispVal where
--    show val = showVal val
--
--showVal :: LispVal -> String
--showVal val =
--  case val of
--    (Atom atom)         -> atom
--    (ValString txt)     -> "\"" ++ txt ++ "\""
--    (Number num)        -> show num
--    (ValBool True)      -> "#t"
--    (ValBool False)     -> "#f"
--    Nil                 -> "'()"
--    (List contents)     -> "(" ++ unwordsListVal contents ++  ")"
----    (Fun _ )        -> "(internal function)"
----    (Lambda _ _)    -> "(lambda function)"

--unwordsListVal :: [LispVal] -> String
--unwordsListVal list = unwords $ showVal <$> list


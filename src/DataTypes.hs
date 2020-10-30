module DataTypes
(
    LispVal(..)
) where

--import Errors


--data LispNum = NumInt Int
--            | NumInteger Integer
--            -- Keep which one ?
--            | NumFloat Float
--            | NumDouble Double
--            --
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
            | List [LispVal]
            -- To replace by  LispNum ??
            | Number Integer
            --
            | ValBool Bool
            | ValString String
--            | Fun IFunc
--            | Lambda IFunc EnvCtx
            | Nil
                deriving (Eq)
--                deriving (Eq, Show)

instance Show LispVal where
    show val = showVal val

showVal :: LispVal -> String
showVal val =
  case val of
    (Atom atom)         -> atom
    (ValString txt)     -> "\"" ++ txt ++ "\""
    (Number num)        -> show num
    (ValBool True)      -> "#t"
    (ValBool False)     -> "#f"
    Nil                 -> "'()"
    (List contents)     -> "(" ++ unwordsListVal contents ++  ")"

unwordsListVal :: [LispVal] -> String
unwordsListVal list = unwords $ showVal <$> list

--    (Fun _ )        -> "(internal function)"
--    (Lambda _ _)    -> "(lambda function)"

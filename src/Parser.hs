module Parser where

import LibParsing
import Control.Applicative
import Errors
import DataTypes

-- TODO: put in lib ?
parseSymbol :: Parser Char
parseSymbol = parseAnyChar "<=>&:|*!#$%*+-/?@^_~"

parseLispValBool :: Parser LispVal
parseLispValBool =
    (parseSpaceLike *> (ValBool True <$ parseString "#t"))
    <|> (parseSpaceLike *> (ValBool False <$ parseString "#f"))

parseLispValString :: Parser LispVal
parseLispValString = do
    _ <- parseChar '"'
    x <- (:) <$> parseNotChar '"' <*> many (parseNotChar '"')
    _ <- parseChar '"' -- <|> Error mismatched "
    return (ValString x)

-- parseLispDataString same as parseLispDataBool

parseLispValInt :: Parser LispVal
parseLispValInt = ValNum <$> parseInteger

parseLispValAtom :: Parser LispVal
parseLispValAtom = do
    firstChar   <- parseLetter <|> parseSymbol
    left        <- many (parseLetter <|> parseDigit <|> parseSymbol)
    return $ Atom $ firstChar:left

parseLispValList :: Parser LispVal
parseLispValList = do
    _ <- parseChar '('
    x <- ValList <$> many (parseLispVal <* many parseSpaceLike)
    _ <- parseChar ')'
    return x

parseQuoted :: Parser LispVal
parseQuoted = do
    quoted <- parseChar '\''*> parseLispVal
    return $ ValList [Atom "quoted", quoted]

parseLispVal :: Parser LispVal
parseLispVal =
    parseLispValBool
    <|> parseLispValAtom
    <|> parseLispValInt
    <|> parseLispValString
    <|> parseQuoted
    <|> parseLispValList
--    <|> parseLispVal
--    parseLispVal

eval :: LispVal -> LispVal
eval (Atom "failed") = Atom "failed"
eval (ValList [Atom "quote", val]) = val
eval val@(ValString _) = val
eval val@(ValBool _) = val
eval val@(ValNum _) = val
eval (ValList (Atom func : args)) = apply func $ map eval args


apply :: String -> [LispVal] -> LispVal
apply func args = maybe (ValBool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [
    ("+", numericBinop (+))
    ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = ValNum $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (ValNum nb) = nb
unpackNum (ValList [n]) = unpackNum n
unpackNum _ = 0

run :: Parser a -> String -> Result a
run (Parser p) str = case p str of
    Right (a, [])   -> Right (a, [])
    Left msg        -> Left msg
--
runP :: String -> Result LispVal
runP str = case runParser parseLispVal str of
    Right (a, [])   -> Right (eval a, [])
    Left msg        -> Left msg


--parseLispDataBool :: Parser LispData
--parseLispDataBool = Value <$> parseLispValBool

--parseLispValInt :: Parser LispNum
--parseLispValInt = Number <$> parseInt

--parseLispValInteger :: Parser LispNum
--parseLispValInteger = NumInteger <$> parseInteger
--
--parseLispValFloat :: Parser LispNum
--parseLispValFloat = NumFloat <$> parseFloat
--
--parseLispValDouble :: Parser LispNum
--parseLispValDouble = NumDouble <$> parseDouble

--
--parseLispValInt :: Parser LispNum
--parseLispValInt = NumInt <$> parseInt
--
--parseLispValInteger :: Parser LispNum
--parseLispValInteger = NumInteger <$> parseInteger
--
--parseLispValFloat :: Parser LispNum
--parseLispValFloat = NumFloat <$> parseFloat
--
--parseLispValDouble :: Parser LispNum
--parseLispValDouble = NumDouble <$> parseDouble

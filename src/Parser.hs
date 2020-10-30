module Parser
(
    run
  , parseExpr
)
where

import Control.Applicative
import LibParsing
import DataTypes
import Eval
import Errors


run :: Parser a -> String -> Result a
run (Parser p) str = case p str of
    Right (a, [])   -> Right (a, [])
    Left msg        -> Left msg
--
parseExpr :: String -> ThrowsError LispVal
parseExpr str = case runParser parseLispVal str of
--    Right (a, [])   -> Right (eval a, [])
--    Left msg        -> Left msg
    Right (a, [])   -> eval a
    Left msg        -> throw $ UnknownError msg


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
    return $ ValList [Atom "quote", quoted]



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

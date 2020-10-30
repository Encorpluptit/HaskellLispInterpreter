module Parser where

import LibParsing
import Control.Applicative
import DataTypes

-- TODO: put in lib ?
parseSymbol :: Parser Char
parseSymbol = parseAnyChar "<=>&:|*!#$%*+-/?@^_~"

parseLispValBool :: Parser LispVal
parseLispValBool =
    (parseSpaceLike *> (parseString "#t" >> return (ValBool True)))
    <|> (parseSpaceLike *> (parseString "#f" >> return (ValBool False)))

--parseLispDataBool :: Parser LispData
--parseLispDataBool = Value <$> parseLispValBool

parseLispValString :: Parser LispVal
parseLispValString = do
    _ <- parseChar '"'
    x <- (:) <$> parseNotChar '"' <*> many (parseNotChar '"')
    _ <- parseChar '"' -- <|> Error mismatched "
    return (ValString x)

-- parseLispDataString same as parseLispDataBool

parseLispValInt :: Parser LispVal
parseLispValInt = Number <$> parseInteger

parseLispValAtom :: Parser LispVal
parseLispValAtom = do
    firstChar   <- parseLetter <|> parseSymbol
    left        <- many (parseLetter <|> parseDigit <|> parseSymbol)
    return $ Atom $ firstChar:left

parseLispVal :: Parser LispVal
parseLispVal =
    parseLispValInt
    <|> parseLispValBool
    <|> parseLispValAtom
    <|> parseLispValString
--    <|> parseLispVal

run :: Parser a -> String -> Result a
run (Parser p) str = case p str of
    Right (a, as)   -> Right (a, as)
    Left msg        -> Left msg


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

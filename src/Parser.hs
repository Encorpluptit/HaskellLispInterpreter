module Parser
  ( parseExpr,
    parseLispVal,
  )
where

import Control.Applicative
import DataTypes
import Eval
import LibParsing

parseExpr :: Env -> String -> ThrowsError (LispVal, Env)
--parseExpr str = case runParser (parseManySpaced parseLispVal) str of
parseExpr env str = case runParser parseLispVal str of
  --    Right (a, [])   -> Right (eval a, [])
  --    Left msg        -> Left msg
  --  TODO: Add throw unknown Error when Right (a, as) ??
  -- TODO: add Env management
  Right (a, []) -> eval env a
  Right (a, "\n") -> eval env a
  Right (a, xs) -> throw $ ParsingError a xs
  Left msg -> throw $ UnknownError msg

parseLispVal :: Parser LispVal
parseLispVal =
  parseLispValBool
    --    <|> parseLispValChar TODO
    <|> parseLispValInt
    <|> parseLispValAtom
    <|> parseLispValString
    <|> parseQuoted
--    <|> parseLispList
    <|> parseLispValList
    <|> parseLispValDottedList

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

parseLispValInt :: Parser LispVal
parseLispValInt = ValNum <$> parseInteger

-- | -----------------------------------------------------------------------------------------------------------------
-- Atom:
--  - First Char: letter or symbol
--  - Rest: sequence of letter, digit or symbol
parseLispValAtom :: Parser LispVal
parseLispValAtom = do
  firstChar <- parseLetter <|> parseSymbol
  left <- many (parseLetter <|> parseDigit <|> parseSymbol)
  return $ Atom $ firstChar : left

-- | -----------------------------------------------------------------------------------------------------------------
-- List:
--parseLispList :: Parser LispVal
--parseLispList = do
--  _ <- parseSpacedChar '('
--  x <- parseLispValList <|> parseLispValDottedList
--  _ <- parseSpacedChar ')'
--  return x

--parseLispValList :: Parser LispVal
--parseLispValList =ValList <$> many (parseLispVal <* many parseSpaceLike)

--parseLispValDottedList :: Parser LispVal
--parseLispValDottedList = do
--  headDotted <- many (parseLispVal <* many parseSpaceLike)
--  tailDotted <- parseSpacedChar '.' *> many parseSpaceLike *> parseLispValInt
--  return $ ValDottedList headDotted tailDotted

parseLispValList :: Parser LispVal
parseLispValList = do
  _ <- parseSpacedChar '('
  x <- ValList <$> many (parseLispVal <* many parseSpaceLike)
  _ <- parseSpacedChar ')'
  return x


parseLispValDottedList :: Parser LispVal
parseLispValDottedList = do
  _ <- parseSpacedChar '('
  headDotted <- many (parseLispVal <* many parseSpaceLike)
  tailDotted <- parseSpacedChar '.' *> parseLispValInt
  _ <- parseSpacedChar ')'
  return $ ValDottedList headDotted tailDotted

parseQuoted :: Parser LispVal
parseQuoted = do
  quoted <- parseChar '\'' *> parseLispVal
  return $ ValList [Atom "quote", quoted]

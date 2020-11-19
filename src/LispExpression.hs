module LispExpression where

import Control.Applicative
import LibParsing
import HalError
import LispNumbers
import Data.Either

data LispExpr
  = Atom String
  | Number LispNumber
  | Cons LispExpr LispExpr
  | Nil
  deriving (Eq, Ord, Show)
--  deriving (Eq, Ord)


--instance Show LispExpr where
--  show (Number nb) = show nb
--  show (Atom atom) = atom
--  show (Cons first Nil) = "(" ++ show first ++ ")"
--  show (Cons first second) = "(" ++ show first ++ " . " ++ show second ++ ")"
--  show Nil = "()"

--  show (Cons first second) = "(" ++ show first ++ " . " ++ show second ++ ")"
--    where
--      showCons (Cons first Nil) = show first ++ ")"
--      showCons (Cons first second) = show first ++ " " ++ showCons second
--      showCons Nil = ")"
--      showCons cons = ". " ++ show cons ++ ")"


--instance Read LispExpr where
--  readsPrec _ = parseLispVal

readContent :: String -> ThrowsError LispExpr [LispExpr]
readContent "" = return []
readContent s = case runParser parseLispExpr s of
  Right (expr, rest) -> (expr:) <$> readContent rest
  _ -> throw $ FileError "Parsing Failed"


parseLispExpr :: Parser LispExpr
parseLispExpr =
     parseSpacedChar '(' *> parseCons
    <|> parseLispNumber
    <|> parseAtom
--  parseLispValBool
--    --    <|> parseLispValChar TODO
--    <|> parseLispValInt
--    <|> parseLispValAtom
--    <|> parseLispValString
--    <|> parseQuoted
--    --    <|> parseLispList
--    <|> parseLispValList
--    <|> parseLispValDottedList

--    <|> parseLispVal
--    parseLispVal



parseCons :: Parser LispExpr
parseCons = Cons <$> car <*> cdr
    where
        car = parseManySpaced parseLispExpr
        cdr  =  parseCons <|> (Nil <$ parseSpacedChar ')')

parseLispNumber :: Parser LispExpr
parseLispNumber = Number . LispInt <$> parseInteger

-- | -----------------------------------------------------------------------------------------------------------------
-- Atom:
--  - First Char: letter or symbol
--  - Rest: sequence of letter, digit or symbol
parseAtom :: Parser LispExpr
parseAtom = do
  firstChar <- parseLetter <|> parseSymbol
  left <- many (parseLetter <|> parseDigit <|> parseSymbol)
  return $ Atom $ firstChar : left

parseSymbol :: Parser Char
parseSymbol = parseAnyChar "<=>&:|*!#$%*+-/?@^_~"



--parseLispValDottedList :: Parser LispExpr
--parseLispValDottedList = do
--  _ <- parseSpacedChar '('
--  headDotted <- many (parseLispVal <* many parseSpaceLike)
--  tailDotted <- parseSpacedChar '.' *> parseLispValInt
--  _ <- parseSpacedChar ')'
--  return $ ValDottedList headDotted tailDotted
--
--
--parseList :: String -> Maybe (Expr, String)
--parseList [] = Nothing
--parseList (')' : xs) = Just (Nil, xs)
--parseList ('.' : xs) =
--  parseExpr (eatSeps xs)
--    >>= \(car, s') -> case eatSeps s' of
--      (')' : s'') -> Just (car, s'')
--      _ -> Nothing
--parseList s =
--  parseExpr (eatSeps s)
--    >>= \(car, s') ->
--      parseList (eatSeps s')
--        >>= \(cdr, s'') -> Just (Cons car cdr, s'')
--
--parseSymb :: String -> (String, String)
--parseSymb [] = ([], [])
--parseSymb s@(x : xs)
--  | isSep x || x `elem` "()" = ([], eatSeps s)
--  | otherwise =
--    let (rest, s') = parseSymb xs
--     in (x : rest, s')


--instance Show LispVal where
--  show (Atom name) = "Atom \"" ++ name ++ "\""
--  show (ValNum num) = "ValNum " ++ show num
--  show (ValList[]) = "ValList []"
--  show (ValList list) = "ValList [" ++ foldl1 ((++) . (++ ", ")) (show <$> list) ++ "]"
--  show (ValDottedList list left) = "ValDottedList (" ++ show list ++ " . " ++ show left ++ ")"
--  show (ValBool bool) = "ValNum " ++ show bool
--  show (ValString str) = "ValString \"" ++ str ++ "\""
--  show (Func _ func) = "Func {Intern Env} " ++ show func

--module LispExpressionParser
--  ( parseExpr,
--    parseLispVal,
--    isSep
--  )
--where
--
--import Control.Applicative
--import DataTypes
--import Eval
--import LibParsing
--import Debug.Trace
--
--parseExpr :: Env -> String -> ThrowsError (LispVal, Env)
----parseExpr str = case runParser (parseManySpaced parseLispVal) str of
--parseExpr env str = case runParser parseLispVal str of
--  --    Right (a, [])   -> Right (eval a, [])
--  --    Left msg        -> Left msg
--  --  TODO: Add throw unknown Error when Right (a, as) ??
--  -- TODO: add Env management
--  Right (a, []) -> eval env a
--  Right (a, "\n") -> eval env a
--  Right (a, xs) -> case all isSep xs of
--    True -> throw $ ParsingError a xs
--    False -> throw $ ParsingError a xs
--        where
--  Left msg -> throw $ UnknownError msg
--
--isSep :: Char -> Bool
--isSep c
--    | c `elem` " \t\n" = True
--    | otherwise = False
----parseExpr :: Env -> String -> ThrowsError (LispVal, Env)
------parseExpr str = case runParser (parseManySpaced parseLispVal) str of
----parseExpr env str = case runParser parseLispVal str of
----  --    Right (a, [])   -> Right (eval a, [])
----  --    Left msg        -> Left msg
----  --  TODO: Add throw unknown Error when Right (a, as) ??
----  -- TODO: add Env management
----  Right (a, []) -> eval env a
----  Right (a, "\n") -> eval env a
----  Right (a, xs) -> throw $ ParsingError a xs
----  Left msg -> throw $ UnknownError msg
--
--parseLispVal :: LispExpressionParser LispVal
--parseLispVal =
--  parseLispValBool
--    --    <|> parseLispValChar TODO
--    <|> parseLispValInt
--    <|> parseLispValAtom
--    <|> parseLispValString
--    <|> parseQuoted
--    --    <|> parseLispList
--    <|> parseLispValList
--    <|> parseLispValDottedList
--
----    <|> parseLispVal
----    parseLispVal
--
---- TODO: put in lib ?
--parseSymbol :: LispExpressionParser Char
--parseSymbol = parseAnyChar "<=>&:|*!#$%*+-/?@^_~"
--
--parseLispValBool :: LispExpressionParser LispVal
--parseLispValBool =
--  (parseSpaceLike *> (ValBool True <$ parseString "#t"))
--    <|> (parseSpaceLike *> (ValBool False <$ parseString "#f"))
--
--parseLispValString :: LispExpressionParser LispVal
--parseLispValString = do
--  _ <- parseChar '"'
--  x <- (:) <$> parseNotChar '"' <*> many (parseNotChar '"')
--  _ <- parseChar '"' -- <|> Error mismatched "
--  return (ValString x)
--
--parseLispValInt :: LispExpressionParser LispVal
--parseLispValInt = ValNum <$> parseInteger
--
---- | -----------------------------------------------------------------------------------------------------------------
---- Atom:
----  - First Char: letter or symbol
----  - Rest: sequence of letter, digit or symbol
--parseLispValAtom :: LispExpressionParser LispVal
--parseLispValAtom = do
--  firstChar <- parseLetter <|> parseSymbol
--  left <- many (parseLetter <|> parseDigit <|> parseSymbol)
--  return $ Atom $ firstChar : left
--
---- | -----------------------------------------------------------------------------------------------------------------
---- List:
---- parseLispList :: Parser LispVal
---- parseLispList = do
----  _ <- parseSpacedChar '('
----  x <- parseLispValList <|> parseLispValDottedList
----  _ <- parseSpacedChar ')'
----  return x
--
----parseLispValList :: Parser LispVal
----parseLispValList =ValList <$> many (parseLispVal <* many parseSpaceLike)
--
----parseLispValDottedList :: Parser LispVal
----parseLispValDottedList = do
----  headDotted <- many (parseLispVal <* many parseSpaceLike)
----  tailDotted <- parseSpacedChar '.' *> many parseSpaceLike *> parseLispValInt
----  return $ ValDottedList headDotted tailDotted
--
--parseLispValList :: Parser LispVal
--parseLispValList = do
--  _ <- parseSpacedChar '('
--  x <- ValList <$> many (parseLispVal <* many parseSpaceLike)
--  _ <- parseSpacedChar ')'
--  return x
--
--parseLispValDottedList :: LispExpressionParser LispVal
--parseLispValDottedList = do
--  _ <- parseSpacedChar '('
--  headDotted <- many (parseLispVal <* many parseSpaceLike)
--  tailDotted <- parseSpacedChar '.' *> parseLispValInt
--  _ <- parseSpacedChar ')'
--  return $ ValDottedList headDotted tailDotted
--
--parseQuoted :: LispExpressionParser LispVal
--parseQuoted = do
--  quoted <- parseChar '\'' *> parseLispVal
--  return $ ValList [Atom "quote", quoted]

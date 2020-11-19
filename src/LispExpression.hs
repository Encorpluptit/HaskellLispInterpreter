module LispExpression where

import Control.Applicative
import HalError
import LibParsing
import LispNumbers

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

type ThrowsLispExprError = ThrowsError LispExpr

parseContent :: String -> ThrowsLispExprError [LispExpr]
parseContent "" = return []
parseContent s = case runParser (parseManySpaced parseLispExpr) s of
  Right (expr, left) -> (expr :) <$> parseContent left
  _ -> throw $ FileError $ "Parsing Failed when parsing: " ++ s

parseLispExpr :: Parser LispExpr
parseLispExpr =
  parseCons
    <|> parseLispNumber
    <|> parseAtom

-- | -----------------------------------------------------------------------------------------------------------------
-- Cons:
parseCons :: Parser LispExpr
parseCons = parseSpacedChar '(' *> nextCons
  where
    nextCons = Cons <$> car <*> cdr
    car = parseManySpaced parseLispExpr
    cdr = nextCons <|> (Nil <$ parseSpacedChar ')')

--parseCons :: Parser LispExpr
--parseCons = Cons <$> car <*> cdr
--    where
--        car = parseManySpaced parseLispExpr
--        cdr  =  parseCons <|> (Nil <$ parseSpacedChar ')')

-- | -----------------------------------------------------------------------------------------------------------------
-- Number:
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

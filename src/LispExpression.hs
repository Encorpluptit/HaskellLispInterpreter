module LispExpression
  ( parseContent,
    parseLispExpr,
    LispExpr(..),
  )
where

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

showLispExpr :: LispExpr -> String
showLispExpr (Number nb) = show nb
showLispExpr (Atom atom) = atom
showLispExpr (Cons first Nil) = "(" ++ showLispExpr first ++ ")"
showLispExpr (Cons first second) = "(" ++ showLispExpr first ++ " . " ++ showLispExpr second ++ ")"
showLispExpr Nil = "()"

parseContent :: String -> Either String [LispExpr]
parseContent "" = return []
parseContent s = case runParser (parseManySpaced parseLispExpr) s of
  Right (expr, left) -> (expr :) <$> parseContent left
  _ -> Left $ "Parsing Failed when parsing: " ++ s

parseLispExpr :: Parser LispExpr
parseLispExpr = parseCons <|> parseLispExprNumber <|> parseAtom

-- | -----------------------------------------------------------------------------------------------------------------
-- Cons:
parseCons :: Parser LispExpr
parseCons = parseSpacedChar '(' *> nextCons
  where
    nextCons = Cons <$> car <*> cdr
    car = parseManySpaced parseLispExpr
    cdr = nextCons <|> (Nil <$ parseSpacedChar ')')
--parseCons = Cons <$> car <*> cdr
--    where
--        car = parseManySpaced parseLispExpr
--        cdr  =  parseCons <|> (Nil <$ parseSpacedChar ')')

-- | -----------------------------------------------------------------------------------------------------------------
-- Number:
parseLispExprNumber :: Parser LispExpr
parseLispExprNumber = Number <$> parseLispNumber
--parseLispExprNumber = Number . LispInt <$> parseInteger

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

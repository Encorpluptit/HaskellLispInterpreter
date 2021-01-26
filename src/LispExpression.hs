module LispExpression
  ( parseContent,
    parseInput,
    printLispExpr,
    LispExpr (..),
    ThrowsLispExprError,
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

type ThrowsLispExprError = ThrowsError LispExpr

printLispExpr :: Bool -> (String -> m ()) -> LispExpr -> m ()
printLispExpr False f = f . showLispExpr
printLispExpr True f = f . show

showLispExpr :: LispExpr -> String
showLispExpr (Number nb) = show nb
showLispExpr (Atom atom) = atom
showLispExpr (Cons first Nil) = "(" ++ showLispExpr first ++ ")"
showLispExpr (Cons first second) = "(" ++ showLispExpr first ++ " . " ++ showLispExpr second ++ ")"
showLispExpr Nil = "()"

parseContent :: String -> ThrowsLispExprError [LispExpr]
parseContent "" = return []
parseContent s = case runParser (parseManySpaced parseLispExpr) s of
  Right (expr, left) -> (expr :) <$> parseContent left
  _ -> throw $ FileError $ "Parsing Failed when parsing: " ++ s

parseInput :: String -> ThrowsLispExprError LispExpr
parseInput "" = return Nil
parseInput s = case runParser (parseManySpaced parseLispExpr) s of
  Right (expr, "") -> return expr
  _ -> throw $ SyntaxError $ "Parsing Failed when parsing: " ++ s

parseLispExpr :: Parser LispExpr
parseLispExpr = parseCons <|> parseLispExprNumber <|> parseManySpaced parseAtom <|> parseQuoted <|> parseNil

-- | -----------------------------------------------------------------------------------------------------------------
-- Cons:
parseCons :: Parser LispExpr
parseCons = parseSpacedChar '(' *> nextCons
  where
    nextCons = Cons <$> car <*> cdr
    car = parseManySpaced parseLispExpr
    cdr = nextCons <|> (Nil <$ parseSpacedChar ')')

-- | -----------------------------------------------------------------------------------------------------------------
-- Number:
parseLispExprNumber :: Parser LispExpr
parseLispExprNumber = Number <$> parseLispNumber

--parseLispExprNumber = Number . LispInt <$> parseInteger

parseNil :: Parser LispExpr
parseNil = parseSpacedChar '(' *> (Nil <$ parseSpacedChar ')')

-- | -----------------------------------------------------------------------------------------------------------------
-- Number:
parseQuoted :: Parser LispExpr
parseQuoted = Cons <$> quoted <*> expr
  where
    quoted = Atom "quote" <$ parseSpacedChar '\''
    expr = Cons <$> parseLispExpr <*> return Nil

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

module Parsing where

import Data.Either
import Control.Applicative

type Error = String
type Result a = Either Error (a , String)

data Parser a = Parser {
    runParser :: String -> Result a
}

parseChar :: Char -> Parser Char
parseChar c = Parser fct
    where
        fct (x:xs)
            | x == c    = Right (c, xs)
            | otherwise = Left $ "ParseChar failed {Left:" ++ xs ++ "}"
        fct [] = Left "ParseChar failed: Empty or End of List"

parseDigit :: Parser Char
parseDigit = parseAnyChar ['0'..'9']

parseFloatDigit :: Parser Char
parseFloatDigit = parseAnyChar ('.':['0'..'9'])

-- Using Alternative <|> (parseOr) to parse a char or the rest of the string
parseAnyChar :: String -> Parser Char
parseAnyChar = foldr ((<|>) . parseChar) failed
    where failed = Parser $ const $ Left "ParseAnyChar failed: Empty or End of List"

-- Using fmap infix notation to read Int from String (ghc understand itself String->Int) on the digits chars parsed by parseSome.
parseUInt :: Parser Int
parseUInt = read <$> some parseDigit

-- Using Alternative Functor to read Int from String (ghc understand itself String->Int) on the digits chars parsed by parseSome.
parseInt :: Parser Int
parseInt = parseNegInt <|> parseUInt
    where
--        parseNegInt = const negate <$> parseChar '-' <*> parseUInt
        parseNegInt = (negate <$ parseChar '-') <*> parseUInt

-- Using fmap infix notation to read Int from String (ghc understand itself String->Int) on the digits chars parsed by parseSome.
parseUFloat :: Parser Float
parseUFloat = (read::String->Float) <$> some parseFloatDigit

-- Using Alternative Functor to read Int from String (ghc understand itself String->Int) on the digits chars parsed by parseSome.
parseFloat :: Parser Float
parseFloat = parseNegFloat <|> parseUFloat
    where
--        parseNegFloat = const negate <$> parseChar '-' <*> parseUFloat
        parseNegFloat = (negate <$ parseChar '-') <*> parseUFloat

parseUDouble :: Parser Double
parseUDouble = read <$> some parseFloatDigit

parseDouble :: Parser Double
parseDouble = parseNegDouble <|> parseUDouble
    where
        parseNegDouble = (negate <$ parseChar '-') <*> parseUDouble

parseSpacedChar :: Char -> Parser Char
parseSpacedChar c = parseSpaced $ parseChar c

parseSpaced :: Parser a -> Parser a
parseSpaced p = many (parseAnyChar "\t ") *> p <* many (parseAnyChar "\t ")

parseTuple :: Parser a -> Parser (a, a)
parseTuple p = openPar *> parseTuple' <* closePar
    where
        parseTuple'     = parseElem <|> Parser (\_ -> Left "Parsing Tuple Failed")
        parseElem       = (\x y -> (x, y)) <$> p <*> (comma *> p)
        openPar         = parseSpaced $ parseChar '('
        closePar        = parseSpaced $ parseChar ')'
        comma           = parseSpaced $ parseChar ','


--instance Functor Parser where
--    fmap f (Parser p) = Parser fct
--        where
--            fct s = case p s of
--                Right (x, xs) -> Right (f x, xs)
--                Left b -> Left b



-- | -----------------------------------------------------------------------------
-- Alternative Functor:
-- Implement:
--      - empty
--      - <|>
--      - some
--      - many
-- | -----------------------------------------------------------------------------

instance Functor Parser where
    fmap f p = do x<-p; return (f x)

--instance Functor Parser where
--    fmap f (Parser p) = Parser fct
--        where
--            fct s = case p s of
--                Right (x, xs) -> Right (f x, xs)
--                Left b -> Left b


-- | -----------------------------------------------------------------------------
-- Alternative Functor:
-- Implement:
--      - empty
--      - <|>
--      - some
--      - many
-- | -----------------------------------------------------------------------------

instance Applicative Parser where
    pure = return
    p1 <*> p2 = do x<-p1; y<-p2; return (x y)

--instance Applicative Parser where
--    pure p = Parser $ \x -> Right (p, x)
--
--    -- Using Applicative to apply Parser p1 AND Parser p2
--    Parser p1 <*> p2 = Parser fct
--        where
--            fct s = case p1 s of
--                Right (f, left) -> case runParser p2 left of
--                    Right (a, left')  -> Right (f a, left')
--                    Left msg        -> Left msg
--                Left msg -> Left msg0


-- | -----------------------------------------------------------------------------
-- Alternative Functor:
-- Implement:
--      - empty
--      - <|>
--      - some
--      - many
-- | -----------------------------------------------------------------------------

instance Alternative Parser where
    empty = Parser $ const $ Left "parser Empty"

    -- Using Alternative to apply Parser p1 OR Parser p2
    Parser p1 <|> Parser p2 = Parser fct
        where
            fct s = case p1 s of
                Left _ -> case p2 s of
                    Right a -> Right a
                    r'        -> r'
                r -> r

    -- Using Functor infix notation to parse with the fct contained in Parser a and apply (:) to the tuple returned by fct
    -- uncurry -> fct that take 2 args (a fct with 2 params and a tuple) to and apply this fct with the elems of this tuple as args of the fct.
    -- uncurry :: (a -> b -> c) -> (a, b) -> c
    -- Here unpack the tuple (a,b) and apply the fct a -> b -> c with a first arg and b second arg
    -- See Functor (fmap)

--    some parser = uncurry (:) <$> fct
--        where
--            fct = (\x y -> (x,y)) <$> parser <*> many parser

    -- Using recursive to parse with Parser a and adding each parsed element to a list of parsed elements
    -- (:) -> fct that take 2 args (an elem and a list) and prepend (insert before) that elem to the list.
    -- (:) :: a -> [a] -> [a]

--    many parser = Parser fct
--        where
--            fct s = case runParser ((:) <$> parser <*> many parser) s of
--                Left _  -> Right ([], s)
--                r       -> r

instance Monad Parser where
    return p = Parser $ \x -> Right (p, x)
    Parser a >>= f = Parser fct
        where
            fct s = case a s of
                Right (x, xs) -> runParser (f x) xs
                Left msg -> Left msg
--    fail msg = Parser (\s -> Left ("Monad Parser [fail]: " ++ msg))

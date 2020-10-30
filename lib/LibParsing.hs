module LibParsing where

import Control.Applicative

-- |-------------------------------------------------------------------------------------------------------------
-- Types Used in Parsing Lib:
--
-- * ParsingError:
--      Simple Alias to string in order to avoid confusion in functions signatures.
--
-- * Result:
--      Alias to an Either Generic Type with:
--          - ParsingError as Left Value (Error).
--          - Tuple with parsed generic type as first value and rest of unparsed string as second value (Success).
--
-- * Parser:
--      Newtype with fields:
--          - runParser:
--              function that take a String as Parameter and return the parsed type wrapped in Result (Either Value).

type ParsingError = String
type Result a = Either ParsingError (a , String)

newtype Parser a = Parser {runParser :: String -> Result a}

-- | -----------------------------------------------------------------------------------------------------------------
-- take a Char as parameter and return a Parser Type that Parse the Requested Character.
-- If the requested Char is not found or if String parameter is empty
parseChar :: Char -> Parser Char
parseChar c = Parser fct
    where
        fct (x:xs)
            | x == c    = Right (x, xs)
            | otherwise = Left $ "ParseChar failed {Left:" ++ xs ++ "}"
        fct [] = Left "ParseChar failed: Empty or End of List"


-- | -----------------------------------------------------------------------------------------------------------------
-- Take a String as parameter and return a Parser Type that parse the first char found in the parameter string.
-- Using Alternative Functor <|> (parseOr) to parse a char or the rest of the string
parseAnyChar :: String -> Parser Char
parseAnyChar = foldr ((<|>) . parseChar) failed
    where failed = Parser $ const $ Left "ParseAnyChar failed: Empty or End of List"


-- | -----------------------------------------------------------------------------------------------------------------
-- take a Char as parameter and return a Parser Type that Parse everything except the Requested Character.
-- If the requested Char is found or if String parameter is empty
parseNotChar :: Char -> Parser Char
parseNotChar c = Parser fct
    where
        fct (x:xs)
            | x /= c    = Right (x, xs)
            | otherwise = Left $ "ParseNotChar failed {Left:" ++ xs ++ "}"
        fct [] = Left "ParseNotChar failed: Empty or End of List"


-- | -----------------------------------------------------------------------------------------------------------------
-- Take a String as parameter and return a Parser Type that parse the first char not found in the parameter string.
-- Using Alternative Functor <|> (parseOr) to parse a char or the rest of the string
parseAnyNotChar :: String -> Parser Char
parseAnyNotChar = foldr ((<|>) . parseNotChar) failed
    where failed = Parser $ const $ Left "ParseAnyNotChar failed: Empty or End of List"


-- | -----------------------------------------------------------------------------------------------------------------
-- Take a String as parameter and return a Parser Type that parse the parameter string if strings matches.
-- Fail if the character is not found.
parseString :: String -> Parser [Char]
parseString (x:xs) = (:) <$> parseChar x <*> parseString xs
parseString _ = return []

-- | -----------------------------------------------------------------------------------------------------------------
-- Take a Escaped String as parameter and return a Parser Type that parse the parameter string.
-- Fail if the character is not found.
parseEscapedString :: Parser [Char]
parseEscapedString = do
            _ <- parseChar '"'
            x <- (:) <$> parseNotChar '"' <*> many (parseNotChar '"')
            _ <- parseChar '"'
            return x

-- | -----------------------------------------------------------------------------------------------------------------
-- 
parseLetter :: Parser Char
parseLetter = parseAnyChar $ ['a'..'z'] ++ ['A'..'Z']

-- | -----------------------------------------------------------------------------------------------------------------
-- Parse digits one by one.
parseDigit :: Parser Char
parseDigit = parseAnyChar ['0'..'9']

-- | -----------------------------------------------------------------------------------------------------------------
-- Parse digits or dot with Alternative Functor (<|>).
parseFloatDigit :: Parser Char
parseFloatDigit = parseDigit <|> parseChar '.'

-- | -----------------------------------------------------------------------------------------------------------------
-- Apply fmap infix notation to read Int from String (ghc understand itself String->Int) on the digits parsed by some.
-- some is implicitly defined with the Alternative Functor implementation
parseUInt :: Parser Int
parseUInt = read <$> some parseDigit

-- | -----------------------------------------------------------------------------------------------------------------
-- Using Alternative Functor to read Int from String (ghc understand itself String->Int) on the digits parsed by some.
-- <$ Notation to apply parseChar, ignore the result and apply the negate function on parseUInt.
-- Same as: { parseNegInt = const negate <$> parseChar '-' <*> parseUInt }
parseInt :: Parser Int
parseInt = parseNegInt <|> parseUInt
    where
        parseNegInt = (negate <$ parseChar '-') <*> parseUInt

-- | -----------------------------------------------------------------------------------------------------------------
-- Apply fmap infix notation to read Integer from String (ghc understand itself String->Integer) on the digits parsed by some.
-- some is implicitly defined with the Alternative Functor implementation
parseUInteger :: Parser Integer
parseUInteger = read <$> some parseDigit

-- | -----------------------------------------------------------------------------------------------------------------
-- Using Alternative Functor to read Integer from String (ghc understand itself String->Integer) on the digits parsed by some.
-- <$ Notation to apply parseChar, ignore the result and apply the negate function on parseUInt.
-- Same as: { parseNegInt = const negate <$> parseChar '-' <*> parseUInt }
parseInteger :: Parser Integer
parseInteger = parseNegInt <|> parseUInteger
    where
        parseNegInt = (negate <$ parseChar '-') <*> parseUInteger

-- | -----------------------------------------------------------------------------------------------------------------
-- Using fmap infix notation to read Float from String (ghc understand itself String->Float) on the chars parsed by some.
-- some is implicitly defined with the Alternative Functor implementation
parseUFloat :: Parser Float
parseUFloat = read <$> some parseFloatDigit

-- | -----------------------------------------------------------------------------------------------------------------
-- Using Alternative Functor to read Float from String (ghc understand itself String->Float) on the digits parsed by some.
-- <$ Notation to apply parseChar, ignore the result and apply the negate function on parseUInt
-- Same as: { parseNegFloat = const negate <$> parseChar '-' <*> parseUFloat }
parseFloat :: Parser Float
parseFloat = parseNegFloat <|> parseUFloat
    where
        parseNegFloat = (negate <$ parseChar '-') <*> parseUFloat

-- | -----------------------------------------------------------------------------------------------------------------
-- Using fmap infix notation to read Double from String (ghc understand itself String->Double) on the chars parsed by some.
-- some is implicitly defined with the Alternative Functor implementation
parseUDouble :: Parser Double
parseUDouble = read <$> some parseFloatDigit

-- | -----------------------------------------------------------------------------------------------------------------
-- Using Alternative Functor to read Double from String (ghc understand itself String->Double) on the digits parsed by some.
-- <$ Notation to apply parseChar, ignore the result and apply the negate function on parseUInt
-- Same as: { parseNegDouble = const negate <$> parseChar '-' <*> parseUDouble }
parseDouble :: Parser Double
parseDouble = parseNegDouble <|> parseUDouble
    where
        parseNegDouble = (negate <$ parseChar '-') <*> parseUDouble

-- | -----------------------------------------------------------------------------------------------------------------
-- Return common space's like characters.
spaceLikeChars :: String
spaceLikeChars = " \\t\\v"

-- | -----------------------------------------------------------------------------------------------------------------
-- Return a parser that handle any space's like characters.
parseSpaceLike :: Parser Char
parseSpaceLike = parseAnyChar spaceLikeChars

-- | -----------------------------------------------------------------------------------------------------------------
-- Return a parser that apply this parser and ignoring space's like chars before and after
parseManySpacedBefore :: Parser a -> Parser a
parseManySpacedBefore p = many parseSpaceLike *> p

-- | -----------------------------------------------------------------------------------------------------------------
-- Return a parser that apply this parser and ignoring space's like chars before and after
parseManySpaced :: Parser a -> Parser a
parseManySpaced p = many parseSpaceLike *> p <* many parseSpaceLike

-- | -----------------------------------------------------------------------------------------------------------------
-- Return a parser that
parseSpacedChar :: Char -> Parser Char
parseSpacedChar c = parseManySpaced $ parseChar c

-- | -----------------------------------------------------------------------------------------------------------------
--
-- TODO: Doc
parseTuple :: Parser a -> Parser (a, a)
parseTuple p = openPar *> parseTuple' <* closePar
    where
        parseTuple'     = parseElem <|> Parser (\_ -> Left "Parsing Tuple Failed")
        parseElem       = (\x y -> (x, y)) <$> p <*> (comma *> p)
        openPar         = parseManySpaced $ parseChar '('
        closePar        = parseManySpaced $ parseChar ')'
        comma           = parseManySpaced $ parseChar ','


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
    p1 <*> p2 = do x <- p1; x <$> p2

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

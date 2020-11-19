module LispNumbers
  ( LispNumber,
    parseLispNumber,
  )
where

import Control.Applicative
import LibParsing

data LispNumber
  = LispInt Integer
  | LispDouble Double
  | LispRational Rational
  deriving (Eq, Ord, Show)

showLispNumber :: LispNumber -> String
showLispNumber (LispInt nb) = show nb
showLispNumber (LispDouble nb) = show nb
showLispNumber (LispRational nb) = show nb

-- | -----------------------------------------------------------------------------------------------------------------
-- Number:
parseLispNumber :: Parser LispNumber
parseLispNumber = int
--parseLispNumber = rational <|> double <|> int
    where
        rational = LispRational <$> parseRational
        double = LispDouble <$> parseDouble
        int = LispInt <$> parseInteger
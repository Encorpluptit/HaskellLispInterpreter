module LispNumbers
  ( LispNumber (..),
    parseLispNumber,
  )
where

import Control.Applicative
import Data.Ratio ((%))
import LibParsing

data LispNumber
  = LispInt Integer
  | LispDouble Double
  --  | LispRational Rational
  deriving (Eq, Ord, Show)

--instance Ord LispNumber where
--    (LispInt x) `compare` (LispInt y) = x `compare` y
--    (LispInt x) `compare` (LispDouble y) = fromIntegral x `compare` y
--    (LispDouble x) `compare` (LispInt y) = x `compare` fromIntegral y
--    (LispDouble x) `compare` (LispDouble y) = x `compare` y

instance Num LispNumber where
  negate (LispInt x) = LispInt (negate x)
  negate (LispDouble x) = LispDouble (negate x)
  --    negate (LispRational x) = LispRational (negate x)

  abs (LispInt x) = LispInt (abs x)
  abs (LispDouble x) = LispDouble (abs x)
  --    abs (LispRational x) = LispRational (abs x)

  signum (LispInt x) = LispInt (signum x)
  signum (LispDouble x) = LispDouble (signum x)
  --    signum (LispRational x) = LispRational (signum x)

  (LispInt x) + (LispInt y) = LispInt (x + y)
  (LispInt x) + (LispDouble y) = LispDouble (fromIntegral x + y)
  (LispDouble x) + (LispInt y) = LispDouble (x + fromIntegral y)
  (LispDouble x) + (LispDouble y) = LispDouble (x + y)
  --    (LispInt x) + (LispRational y) =
  --    (LispDouble x) + (LispRational y) =
  --    (LispRational x) + (LispInt y) =
  --    (LispRational x) + (LispDouble y) =

  (LispInt x) - (LispInt y) = LispInt (x - y)
  (LispInt x) - (LispDouble y) = LispDouble (fromIntegral x - y)
  (LispDouble x) - (LispInt y) = LispDouble (x - fromIntegral y)
  (LispDouble x) - (LispDouble y) = LispDouble (x - y)

  (LispInt x) * (LispInt y) = LispInt (x * y)
  (LispInt x) * (LispDouble y) = LispDouble (fromIntegral x * y)
  (LispDouble x) * (LispInt y) = LispDouble (x * fromIntegral y)
  (LispDouble x) * (LispDouble y) = LispDouble (x * y)

  fromInteger = LispInt

--    fromInte = LispRational
--    fromRational = LispRational

instance Enum LispNumber where
  toEnum x = toEnum x
  fromEnum x = fromEnum x

--    succ (LispInt x) = succ x
--    succ (LispDouble x) = succ x
--    succ (LispRational x) = x
--    quotRem

instance Real LispNumber where
  toRational (LispInt x) = toRational x
  toRational (LispDouble x) = toRational x

----    toRational (LispRational x) = x
--
instance Integral LispNumber where
  toInteger (LispInt x) = toInteger x
  toInteger (LispDouble x) = round x
  ----    toInteger (LispRational x) = x

  quotRem (LispInt x) (LispInt y) = (LispInt (x `div` y), LispInt (x `mod` y))
  quotRem (LispDouble x) (LispInt y) =
    (LispDouble (fromIntegral $ round x `div` y), LispDouble (fromIntegral $ round x `mod` y))
  quotRem (LispInt x) (LispDouble y) =
    (LispDouble (fromIntegral $ x `div` round y), LispDouble (fromIntegral $ x `mod` round y))
  quotRem (LispDouble x) (LispDouble y) =
    (LispDouble (fromIntegral $ round x `div` round y), LispDouble (fromIntegral $ round x `mod` round y))

----    toInteger (LispRational x) = x
--
----    (LispInt x) `div` (LispInt y) = LispInt (x `div` y)
----    (LispInt x) `div` (LispDouble y) = LispDouble (fromIntegral x / y)
----    (LispDouble x) `div` (LispInt y) = LispDouble (x / fromIntegral y)
----    (LispDouble x) `div` (LispDouble y) = LispDouble (x / y)

showLispNumber :: LispNumber -> String
showLispNumber (LispInt nb) = show nb
showLispNumber (LispDouble nb) = show nb

--showLispNumber (LispRational nb) = show nb

-- | -----------------------------------------------------------------------------------------------------------------
-- Number:
parseLispNumber :: Parser LispNumber
parseLispNumber = double <|> int
  where
    --parseLispNumber = rational <|> double <|> int

    --        rational = LispRational <$> parseRational
    double = LispDouble <$> parseStrictDouble
    int = LispInt <$> parseInteger

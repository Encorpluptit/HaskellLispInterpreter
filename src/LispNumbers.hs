module LispNumbers where

data LispNumber =
    LispInt Integer
        deriving (Eq, Ord)
        
instance Show LispNumber where
    show (LispInt nb) = show nb 
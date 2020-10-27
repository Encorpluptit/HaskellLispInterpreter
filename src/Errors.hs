module Errors where

-- | -----------------------------------------------------------------------------------------------------------------
-- ressources
-- https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/10_Error_Handling

data HALError = UnknownError String
                | SyntaxError String
                | KeywordError String

instance Show HALError where
    show (UnknownError msg) = "Unknown Error: " ++ msg
    show (SyntaxError msg)  = "Synthax Error: " ++ msg
    show (KeywordError msg) = "Unkown Keyword: " ++ msg  
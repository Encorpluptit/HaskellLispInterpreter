module Builtins
(
    primitives
)
where

import DataTypes
import qualified Data.Map as Map

-- | -----------------------------------------------------------------------------------------------------------------
-- Environment:

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [
    ("+", numericBinop (+))
    ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = ValNum $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (ValNum nb) = nb
unpackNum (ValList [n]) = unpackNum n
unpackNum _ = 0

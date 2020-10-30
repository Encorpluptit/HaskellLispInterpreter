module Builtins
(
    builtins
)
where

import DataTypes
import Errors
-- | TODO: ??
--import qualified Data.Map as Map

-- | -----------------------------------------------------------------------------------------------------------------
-- Environment:

type BinaryOperator a = (a -> a -> a) 

builtins :: [(String, [LispVal] -> ThrowsError LispVal)]
builtins = [
    ("+", numericBinOp "+" (+)),
    ("-", numericBinOp "-" (-)),
    ("*", numericBinOp "*" (*)),
    ("/", numericBinOp "/" div)
    ]


-- | -----------------------------------------------------------------------------------------------------------------
-- * foldl1:
--  http://zvon.org/other/haskell/Outputprelude/foldl1_f.html
-- * mapM:
--  http://zvon.org/other/haskell/Outputprelude/mapM_f.html
numericBinOp :: String -> BinaryOperator Integer -> [LispVal] -> ThrowsError LispVal
numericBinOp op _ [] = throw $ NbArgsError op 2 (ValList [])
numericBinOp op _ [val] = throw $ NbArgsError op 2 val
-- | TODO: manage op between != types
-- | Equivalents:
--numericBinOp op params = mapM unpackNum params >>= return . ValNum . foldl1 op
numericBinOp op fct params = ValNum . foldl1 fct <$> mapM (unpackNum op) params

unpackNum :: String -> LispVal -> ThrowsError Integer
unpackNum _ (ValNum nb) = return nb
unpackNum s (ValList [n]) = unpackNum s n
unpackNum s err = throw $ TypeError "mismatch" err

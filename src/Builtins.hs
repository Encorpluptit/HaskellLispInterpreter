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

type BoolBinaryOperator a = (a -> a -> Bool)

builtins :: [(String, [LispVal] -> ThrowsError LispVal)]
builtins = [
    ("+", numericBinaryOp "+" (+)),
    ("-", numericBinaryOp "-" (-)),
    ("*", numericBinaryOp "*" (*)),
--    TODO: interference with escape character used in scheme
--    ("/", numericBinaryOp "/" div),
    ("div", numericBinaryOp "div" div),
    ("mod", numericBinaryOp "mod" mod),
    ("remainder", numericBinaryOp "remainder" rem),
    ("quotient", numericBinaryOp "quotient" quot)
    ]


-- | -----------------------------------------------------------------------------------------------------------------
-- * foldl1:
--  http://zvon.org/other/haskell/Outputprelude/foldl1_f.html
-- * mapM:
--  http://zvon.org/other/haskell/Outputprelude/mapM_f.html
numericBinaryOp :: String -> BinaryOperator Integer -> [LispVal] -> ThrowsError LispVal
numericBinaryOp op _ [] = throw $ NbArgsError op 2 (ValList [])
numericBinaryOp op _ [val] = throw $ NbArgsError op 2 val
-- | TODO: manage op between != types
-- | Equivalents:
--numericBinOp op params = mapM unpackNum params >>= return . ValNum . foldl1 op
numericBinaryOp op fct params = ValNum . foldl1 fct <$> mapM (unpackNumeric op) params

unpackNumeric :: String -> LispVal -> ThrowsError Integer
unpackNumeric _ (ValNum nb) = return nb
unpackNumeric s (ValList [n]) = unpackNumeric s n
-- TODO: improve error reporting
unpackNumeric s err = throw $ TypeError "mismatch" err


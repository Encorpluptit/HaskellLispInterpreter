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
-- Scheme Reference:
--  - https://schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html

type BinaryOperator a = (a -> a -> a)

type BoolBinaryOperator a = (a -> a -> Bool)

builtins :: [(String, [LispVal] -> ThrowsError LispVal)]
builtins = [
    ("+", integerBinaryOp "+" (+)),
    ("-", integerBinaryOp "-" (-)),
    ("*", integerBinaryOp "*" (*)),
--    TODO: interference with escape character used in scheme
--    ("/", numericBinaryOp "/" div),
    ("div", integerBinaryOp "div" div),
    ("mod", integerBinaryOp "mod" mod),
    ("remainder", integerBinaryOp "remainder" rem),
    ("quotient", integerBinaryOp "quotient" quot),
    ("number?", unaryOp "quotient" isNumber),
    ("bool?", unaryOp "quotient" isBool),
    ("list?", unaryOp "quotient" isList),
    ("string?", unaryOp "quotient" isString),
    ("symbol?", unaryOp "quotient" isAtom)
    ]


-- | -----------------------------------------------------------------------------------------------------------------
-- * foldl1:
--  http://zvon.org/other/haskell/Outputprelude/foldl1_f.html
-- * mapM:
--  http://zvon.org/other/haskell/Outputprelude/mapM_f.html
integerBinaryOp :: String -> BinaryOperator Integer -> [LispVal] -> ThrowsError LispVal
integerBinaryOp op _ [] = throw $ NbArgsError op 2 []
integerBinaryOp op _ [val] = throw $ NbArgsError op 2 [val]
-- | TODO: manage op between != types (replace BinaryOperator Integer -> BinaryOperator LispNum ?)
-- | Equivalents:
--numericBinOp op params = mapM unpackNum params >>= return . ValNum . foldl1 op
integerBinaryOp op fct params = ValNum . foldl1 fct <$> mapM (unpackNumeric op) params

unpackNumeric :: String -> LispVal -> ThrowsError Integer
unpackNumeric _ (ValNum nb) = return nb
unpackNumeric s (ValList [n]) = unpackNumeric s n
-- TODO: improve error reporting or manage several types
unpackNumeric s err = throw $ TypeError "mismatch" err

unaryOp :: String -> (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp _ fct [val]     = return (fct val)
unaryOp procedure _ args   = throw $ NbArgsError procedure 1 args


-- | -----------------------------------------------------------------------------------------------------------------
-- No Need for
isNumber :: LispVal -> LispVal
isNumber (ValNum _)    = ValBool True
isNumber _             = ValBool False

isBool :: LispVal -> LispVal
isBool (ValBool _)  = ValBool True
isBool _            = ValBool False

isList :: LispVal -> LispVal
isList (ValList _)  = ValBool True
isList _            = ValBool False

isString :: LispVal -> LispVal
isString (ValString _)  = ValBool True
isString _              = ValBool False

isAtom :: LispVal -> LispVal
isAtom (Atom _) = ValBool True
isAtom _        = ValBool False


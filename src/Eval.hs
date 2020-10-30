module Eval where

import DataTypes
import Builtins
import Errors

eval :: LispVal -> ThrowsError LispVal
eval (ValList [Atom "quote", val]) = return val
eval val@(ValString _) = return val
eval val@(ValBool _) = return val
eval val@(ValNum _) = return val
-- | Equivalents:
--eval (ValList (Atom func : args)) = apply func $ map eval args
eval (ValList (Atom func : args)) = mapM eval args >>= apply func
--eval syntaxError = throw $ KeywordError syntaxError
eval syntaxError = throw $ KeywordError syntaxError



-- | -----------------------------------------------------------------------------------------------------------------
-- * lookup:
-- http://zvon.org/other/haskell/Outputprelude/lookup_f.html
apply :: String -> [LispVal] -> ThrowsError LispVal
-- | Equivalents:
--apply func args = case lookup func builtins of
--    Just funcToApply    -> return $ ($args) funcToApply
--    Nothing             -> throw $ BuiltinError func args
apply func args =
    maybe
    (throw $ BuiltinError func args)
    ($ args)
    (lookup func builtins)

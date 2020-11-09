module Eval
  ( eval,
  )
where

import Builtins
import DataTypes
import Errors

eval :: LispVal -> ThrowsError LispVal
eval (ValList [Atom "quote", val]) = return val
eval val@(ValString _) = return val
eval val@(ValBool _) = return val
eval val@(ValNum _) = return val
eval (ValList (Atom "cond" : val)) = cond val
eval (ValList [Atom "if", condition, validated, other]) = do
  condEvaluated <- eval condition
  case condEvaluated of
    ValBool True -> eval validated
    _ -> eval other
eval (ValList (Atom func : args)) = mapM eval args >>= apply func
--eval syntaxError = throw $ KeywordError syntaxError
eval syntaxError = throw $ KeywordError syntaxError

-- | -----------------------------------------------------------------------------------------------------------------
-- * lookup:
-- http://zvon.org/other/haskell/Outputprelude/lookup_f.html
apply :: String -> [LispVal] -> ThrowsError LispVal

-- | Equivalents:
-- apply func args = case lookup func builtins of
--    Just funcToApply    -> return $ ($args) funcToApply
--    Nothing             -> throw $ BuiltinError func args
apply func args =
  maybe
    (throw $ BuiltinError func args)
    ($ args)
    (lookup func builtins)

-- TODO: Fix circle import AND FIX
cond :: [LispVal] -> ThrowsError LispVal
cond (ValList [condition, validated] : other) = do
  resEval <- eval condition
  res <- unpackBoolean "cond" resEval
  result res
  where
    result res = if res then eval validated else cond other
--        where result resEval = if unpackBoolean "cond" resEval then eval validated else cond other

cond args@(_ : _) = throw $ NbArgsError "cond" 2 args
cond _ = throw $ SyntaxError "Error in cond"

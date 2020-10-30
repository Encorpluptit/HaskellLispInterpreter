module Eval where

import DataTypes
import Builtins

eval :: LispVal -> LispVal
eval (Atom "failed") = Atom "failed"
eval (ValList [Atom "quote", val]) = val
eval val@(ValString _) = val
eval val@(ValBool _) = val
eval val@(ValNum _) = val
eval (ValList (Atom func : args)) = apply func $ map eval args


apply :: String -> [LispVal] -> LispVal
apply func args = maybe (ValBool False) ($ args) $ lookup func primitives

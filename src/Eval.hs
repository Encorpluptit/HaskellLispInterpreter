module Eval
  ( eval,
  )
where

import Builtins
import Data.List (genericLength)
import DataTypes
import Debug.Trace
import Environment
import HalError
import HalErrorsMonad

eval :: Env -> LispVal -> ThrowsError (LispVal, Env)
--eval env (Atom ident) = getEnvVar env ident
eval env (ValList [Atom "quote", val]) = return (val, env)
eval env val@(ValString _) = return (val, env)
eval env val@(ValBool _) = return (val, env)
eval env val@(ValNum _) = return (val, env)
eval env (ValList (Atom "define" : args)) = define env args
eval env (ValList (Atom "cond" : args)) = cond env args
--eval env (ValList [Atom "#t", val]) = evalBool env val isTrueExpr
--eval env (ValList [Atom "#f", val]) = evalBool env val isFalseExpr
eval env (ValList [Atom "if", condition, validated, other]) = do
  condEvaluated <- eval env condition
  case condEvaluated of
    (ValBool True, newEnv) -> eval newEnv validated
    _ -> eval env other
-- TODO: check env here
eval env (ValList (Atom "lambda" : args)) = lambda env args
--eval env (ValList (Atom func : args)) = trace (show env) mapM (eval env) args >>= apply func env . map fst
eval env (ValList (func : args)) = evalFunc env (ValList (func : args))
eval _ syntaxError = throw $ KeywordError syntaxError

evalFunc' :: Env -> LispVal -> [LispVal] -> ThrowsError (LispVal, Env)
evalFunc' env (Func closure (LispFct f)) args = do
  result <- f (mergeEnvs env closure) args
  return (result, env)
evalFunc' _ notFunc _ = throw $ NotFunction "Cannot evaluate" notFunc

evalFunc :: Env -> LispVal -> ThrowsError (LispVal, Env)
evalFunc env (ValList (headExpr : argExprs)) = do
  (result, _) <- eval env headExpr
  args <- evalArgs [] env argExprs
  evalFunc' env result args
evalFunc _ notFunc = throw $ NotFunction "Cannot evaluate" notFunc

-- | -----------------------------------------------------------------------------------------------------------------
apply :: String -> Env -> [LispVal] -> ThrowsError (LispVal, Env)
apply func env args = do
  --  fct <- trace (show args) getEnvFunc env func
  fct <- getEnvFunc env func
  res <- fct args
  return (res, env)

-- TODO: refacto
evalArgs :: [LispVal] -> Env -> [LispVal] -> ThrowsError [LispVal]
evalArgs acc _ [] = return acc
evalArgs acc env (first : left) = do
  (result, _) <- eval env first
  evalArgs (acc ++ [result]) env left

-- TODO: Fix circle import AND FIX
--cond :: Env -> [LispVal] -> ThrowsError (LispVal, Env)
--cond env (ValList [condition, validated] : other) = do
--  (resEval, _) <- eval env (ValList [condition, validated])
--  res <- unpackBoolean "cond" resEval
--  result res
--  where
--    result res = if res then eval env validated else cond env other
--cond _ args@(_ : _) = throw $ NbArgsError "cond" 2 args
--cond _ _ = throw $ SyntaxError "Error in cond"
--cond :: Env -> [LispVal] -> ThrowsError (LispVal, Env)
--cond env (ValList [condition, validated] : other) = do
--  (resEval, _) <- eval env condition
--  res <- unpackBoolean "cond" resEval
--  result res
--  where
--    result res = if res then eval env validated else cond env other
--cond _ args@(_ : _) = throw $ NbArgsError "cond" 2 args
--cond _ _ = throw $ SyntaxError "Error in cond"0

cond :: Env -> [LispVal] -> ThrowsError (LispVal, Env)
cond env nature@((ValList [Atom a, validated]) : other)
  | a == "#t" = parseBool isTrueExpr
  | a == "#f" = parseBool isFalseExpr
  | otherwise = cond env nature
  where
    parseBool fct = do
      (res, newEnv) <- eval env validated
      (if fct res then return (res, newEnv) else cond env other)
cond env (ValList [condition, validated] : other) = do
  (resEval, newEnv) <- eval env condition
  res <- unpackBoolean "cond" resEval
  (if res then eval newEnv validated else cond env other)
-- TODO: change Error ?
cond _ a = trace (show a) throw $ SyntaxError "Error in cond"

-- | define keyword used for creating global variable name bindings. This
--  operation may overwrite existing bindings, if present.
--
--  Syntax: (define <name> <expr>)
define :: Env -> [LispVal] -> ThrowsError (LispVal, Env)
define _ [] = throw $ NbArgsError "define" 2 []
define _ [a] = throw $ NbArgsError "define" 2 [a]
define envMap [Atom name, expr] = do
  (result, _) <- eval envMap expr
  return (Atom name, addEnvVar envMap name result)
define envMap [ValList (Atom name : args), body] = do
  return (Atom name, addEnvFunc envMap name fct)
  where
    fct callArgs
      | length callArgs /= length args = throw $ NbArgsError name (genericLength args) callArgs
      | otherwise = do
        argNames <- getArgNames [] args
        (res, _) <- eval (internEnv argNames) body
        return res
      where
        internEnv argNames = addVarsToEnv envMap (zip argNames callArgs)
define _ [_, _] = throw $ SyntaxError "define (define <name> <expr>)"
define _ args = throw $ NbArgsError "define" 2 args

-- TODO: refacto
getArgNames :: [Identifier] -> [LispVal] -> ThrowsError [Identifier]
getArgNames acc [] = trace (show acc) return acc
getArgNames acc ((Atom ident) : exprs) = getArgNames (acc ++ [ident]) exprs
getArgNames _ (expr : _) = throw . SyntaxError $ "Invalid argument identifier `" ++ show expr ++ "` in lambda expression"

evalBool :: Env -> LispVal -> (LispVal -> Bool) -> ThrowsError (LispVal, Env)
evalBool env expr boolFct = do
  (res, _) <- eval env expr
  return (ValBool $ boolFct res, env)

isTrueExpr :: LispVal -> Bool
isTrueExpr (ValBool True) = True
isTrueExpr (ValList []) = False
isTrueExpr (ValNum 0) = False
isTrueExpr (ValNum _) = True
isTrueExpr (ValList _) = True
isTrueExpr _ = False

isFalseExpr :: LispVal -> Bool
isFalseExpr = not . isTrueExpr

-- | lambda keyword for creating anonymous functions within Lisp code. This
--  includes an (admittedly rudimentary) implementation of closures, in the sense
--  that the environment in which an anonymous function is created is stored
--  in the function returned by lambda.
--
--  Syntax: (lambda (<arg-ids> ...) <expr>)
lambda :: Env -> [LispVal] -> ThrowsError (LispVal, Env)
lambda _ [] = throw $ NbArgsError "lambda" 2 []
lambda _ [a] = throw $ NbArgsError "lambda" 2 [a]
lambda env [ValList argExprs, expr] = do
  argNames <- getArgNames [] argExprs
  return (Func env $ LispFct $ lambda' env argNames expr, env)
lambda _ [_, _] = throw $ SyntaxError "(lambda (<arg-ids> ...) <expr>"
lambda _ args = throw $ NbArgsError "lambda" 2 args

lambda' :: Env -> [Identifier] -> LispVal -> (Env -> [LispVal] -> ThrowsError LispVal)
lambda' env argNames expr callerEnv args
  | length argNames /= length args = trace (show argNames) throw $ NbArgsError "#<procedure>" (genericLength args) args
  | otherwise = do
    (result, _) <- eval internEnv expr
    return result
  where
    internEnv = mergeEnvs (addVarsToEnv env (zip argNames args)) callerEnv

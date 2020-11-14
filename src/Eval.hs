module Eval
  ( eval,
  )
where

import Builtins
import Data.List (genericLength)
import DataTypes
import Debug.Trace
import Environment
import Errors

eval :: Env -> LispVal -> ThrowsError (LispVal, Env)
eval env (Atom ident) = getEnvVar env ident
eval env (ValList [Atom "quote", val]) = return (val, env)
eval env val@(ValString _) = return (val, env)
eval env val@(ValBool _) = return (val, env)
eval env val@(ValNum _) = return (val, env)
eval env (ValList (Atom "define" : val)) = define env val
eval env (ValList (Atom "cond" : val)) = cond env val
--eval env (ValList [Atom "#t", val]) = evalBool env val isTrueExpr
--eval env (ValList [Atom "#f", val]) = evalBool env val isFalseExpr
eval env (ValList [Atom "if", condition, validated, other]) = do
  condEvaluated <- eval env condition
  case condEvaluated of
    (ValBool True, newEnv) -> eval newEnv validated
    _ -> eval env other
-- TODO: check env here
--eval env (ValList (Atom func : args)) = mapM (eval env) args >>= apply func env . map fst
eval env (ValList (Atom func : args)) = mapM (eval env) args >>= apply func env . map fst
eval _ syntaxError = throw $ KeywordError syntaxError

-- | -----------------------------------------------------------------------------------------------------------------
-- * lookup:
-- http://zvon.org/other/haskell/Outputprelude/lookup_f.html
-- apply :: String -> Env -> [LispVal] -> ThrowsError (LispVal, Env)
apply :: String -> Env -> [LispVal] -> ThrowsError (LispVal, Env)
--
---- | Equivalents:
--apply func env args = case Map.lookup func (toMap env) of
--    Just funcToApply    -> do
--        res <- ($args) funcToApply
--        return (res, env)
--    Nothing -> throw $ BuiltinError func args
--apply func env args = case Map.lookup func (toMap env) of
--    Just funcToApply    -> do
--        res <- ($args) funcToApply
--        return (res, env)
--    Nothing -> case Map.lookup func (toMap env) of
--        Just val    ->  return (val ,env)
--        Nothing     ->  throw $ UnboundVar func
apply func env args = do
  fct <- trace (show args) getEnvFunc env func
  res <- fct args
  return (res, env)

--apply func env args =
--  maybe
--    (throw $ BuiltinError func args)
--    ($ args)
--    (lookup func builtins)

--evalArgExprsRecurse :: [HData] -> Env -> [Expr] -> ThrowsError [HData]
--evalArgExprsRecurse acc env [] = return acc
--evalArgExprsRecurse acc env (expr:exprs) = do
--    (result, _) <- evalExpr env expr
--    evalArgExprsRecurse (acc ++ [result]) env exprs
--    -- Ignoring any new environment, since hasp expressions cannot affect
--    -- name bindings at a higher-level scope, apart from define, which is
--    -- valid only as a top-level expression.
--
--evalArgExprs :: Env -> [Expr] -> ThrowsError [HData]
--evalArgExprs = evalArgExprsRecurse []
--
--evalFunc :: Env -> HData -> [HData] -> ThrowsError (HData, Env)
--evalFunc env (HFunc closure f) args = do
--    result <- f (Env $ Map.union (toMap env) (toMap closure)) args
--    return (result, env)
--evalFunc _ notFunc _ = throw . TypeError $ "Cannot evaluate `" ++ show notFunc ++ "`"
--
--evalFuncCall :: Env -> Expr -> ThrowsError (HData, Env)
--evalFuncCall env (List (headExpr:argExprs)) = do
--    (result, _) <- evalExpr env headExpr
--    args <- evalArgExprs env argExprs
--    evalFunc env result args

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
--cond _ _ = throw $ SyntaxError "Error in cond"

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
        internEnv argNames = getSubEnv envMap (zip argNames callArgs)
define _ [_, _] = throw $ SyntaxError "define (define <name> <expr>)"
define _ args = throw $ NbArgsError "define" 2 args

getArgNames :: [Identifier] -> [LispVal] -> ThrowsError [Identifier]
getArgNames acc [] = return acc
getArgNames acc ((Atom ident) : exprs) = getArgNames (acc ++ [ident]) exprs
getArgNames _ (expr : _) = throw . SyntaxError $ "Invalid argument identifier `" ++ show expr ++ "` in lambda expression"

-- TODO: Fix circle import AND FIX
--cond :: [LispVal] -> ThrowsError LispVal
--cond nature@((ValList [Atom a, validated]) : other)
--    | a == "#t" = parseBool isTrueExpr
--    | a == "#f" = parseBool isFalseExpr
--    | otherwise = cond nature
--        where
--            parseBool fct = do
--                res <- eval validated
--                (if fct res then return res else cond other)
--cond (ValList [condition, validated] : other) = do
--  resEval <- eval condition
--  res <- unpackBoolean "cond" resEval
--  (if res then eval validated else cond other)
--cond a = trace (show a) throw $ SyntaxError "Error in cond"

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

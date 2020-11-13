module Eval
  ( eval,
  )
where

import Builtins
import DataTypes
import Errors
import Environment
import qualified Data.Map as Map
import Debug.Trace

eval :: Env -> LispVal -> ThrowsError (LispVal, Env)
eval env (ValList [Atom "quote", val]) = return (val, env)
eval env val@(ValString _) = return (val, env)
eval env val@(ValBool _) = return (val, env)
eval env val@(ValNum _) = return (val, env)
eval env (ValList (Atom "cond" : val)) = cond env val
eval env (ValList [Atom "if", condition, validated, other]) = do
  condEvaluated <- eval env condition
  case condEvaluated of
    (ValBool True, newEnv) -> eval newEnv validated
    _ -> eval env other
-- TODO: check env here
eval env (ValList (Atom func : args)) = mapM (eval env) args >>= apply func env . map fst
--eval env (ValList (Atom func : args)) = do
--    res <- mapM (eval env) args
--    apply func env (map fst res)
eval _ syntaxError = throw $ KeywordError syntaxError

-- | -----------------------------------------------------------------------------------------------------------------
-- * lookup:
-- http://zvon.org/other/haskell/Outputprelude/lookup_f.html
--apply :: String -> Env -> [LispVal] -> ThrowsError (LispVal, Env)
apply :: String -> Env -> [LispVal] -> ThrowsError (LispVal, Env)
--
---- | Equivalents:
apply func env args = case Map.lookup func (toMap env) of
    Just funcToApply    -> do
        res <- ($args) funcToApply
        return (res, env)
    Nothing -> throw $ BuiltinError func args
--apply func args =
--  maybe
--    (throw $ BuiltinError func args)
--    ($ args)
--    (lookup func (toMap builtins))


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
cond :: Env -> [LispVal] -> ThrowsError (LispVal, Env)
cond env (ValList [condition, validated] : other) = do
  (resEval, _) <- eval env condition
  res <- unpackBoolean "cond" resEval
  result res
  where
    result res = if res then eval env validated else cond env other
--        where result resEval = if unpackBoolean "cond" resEval then eval validated else cond other

cond _ args@(_ : _) = throw $ NbArgsError "cond" 2 args
cond _ _ = throw $ SyntaxError "Error in cond"

-- |define keyword used for creating global variable name bindings. This
-- operation may overwrite existing bindings, if present.
--
-- Syntax: (define <name> <expr>)
--define :: Env -> [LispVal] -> ThrowsError (LispVal, Env)
--define _ [] = throw $ NbArgsError "define" 2 []
--define _ [a] = throw $ NbArgsError "define" 2 [a]
--define (Env envMap) [Atom name, expr] = do
----    (result, _) <- eval (Env envMap) expr
--    result <- eval (Env envMap) expr
--    -- TODO: replace ValList by HFunc ?
--    return (ValList [], Env $ Map.insert name result envMap)

define _ [_, _] = throw $ SyntaxError "define (define <name> <expr>)"
define _ args = throw $ NbArgsError "define" 2 args

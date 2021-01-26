module HalEvaluation where

import HalDataTypes
import HalEnvironment
import HalError
import LispExpression

evalLispValue :: Env -> LispExpr -> ThrowsHalExprError HalExpr
evalLispValue _ (Atom "#t") = return $ Bool True
evalLispValue _ (Atom "#f") = return $ Bool False
evalLispValue env (Atom ident) = lookupEnv ident env
evalLispValue env (Cons (Atom "+") body) = evalFunc env (Atom "+") body
evalLispValue env (Cons (Atom "div") body) = evalFunc env (Atom "div") body
evalLispValue _ (Cons (Atom _) _) = throw $ UnknownError "(Cons (Atom [define, lambda, etc.]) _) not implemented in interpretLispExpr"
evalLispValue _ (Cons _ _) = throw $ UnknownError "(Cons expr expr) not implemented in interpretLispExpr"
evalLispValue _ e = return $ Value e

evalHalExpr :: Env -> HalExpr -> ThrowsHalExprError HalExpr
evalHalExpr env (Func f args) = do
  fct <- evalHalExpr env f
  evaluatedArgs <- traverse (evalHalExpr env) args
  apply env fct evaluatedArgs
evalHalExpr env (Condition cond ifthen ifelse) = evalHalExpr env cond >>= evalIf
  where
    evalIf (Bool False) = evalHalExpr env ifelse
    evalIf _ = evalHalExpr env ifthen
evalHalExpr env (Ident name) = case unpackError $ lookupEnv name env of
  Right res -> return res
  _ -> throw $ UnboundVar (Ident name)
evalHalExpr env (Lambda name params body _) =
  return $ Lambda name params body env
evalHalExpr _ expr = return expr

apply :: Env -> HalExpr -> [HalExpr] -> ThrowsHalExprError HalExpr
apply _ (Builtin (Built f)) args = f args
apply _ func _ = throw $ NotFunction func

evalFunc :: Env -> LispExpr -> LispExpr -> ThrowsHalExprError HalExpr
evalFunc env f body = Func <$> evalLispValue env f <*> evalBody env body

evalBody :: Env -> LispExpr -> ThrowsHalExprError [HalExpr]
evalBody env expr = sequence =<< evalCons (evalLispValue env) expr

evalCons :: (LispExpr -> a1) -> LispExpr -> ThrowsHalExprError [a1]
evalCons _ Nil = return []
evalCons f (Cons expr' rest) = (f expr' :) <$> evalCons f rest
evalCons _ expr' = throw $ SyntaxError $ "Malformed Cons: " ++ show expr'

ifStatement :: Env -> LispExpr -> ThrowsHalExprError HalExpr
ifStatement env (Cons cond (Cons validated (Cons other Nil))) =
  Condition <$> evalLispValue env cond <*> evalLispValue env validated <*> evalLispValue env other

condStatement :: Env -> LispExpr -> ThrowsHalExprError HalExpr
--cond _ Nil = Left "Compile: cond without 'catchall' ending..."
condStatement env (Cons (Cons (Atom "else") (Cons body Nil)) Nil) = evalLispValue env body
condStatement env (Cons (Cons (Atom "#t") (Cons body Nil)) Nil) = evalLispValue env body
condStatement env (Cons (Cons cond (Cons body Nil)) rest) =
  Condition <$> evalLispValue env cond <*> evalLispValue env body <*> condStatement env rest
condStatement _ e0 = throw $ SyntaxError ("can't parse: " ++ show e0)

--module Eval
--  ( eval,
--  )
--where
--
--import Builtins
--import Control.Applicative
--import Data.List (genericLength)
--import DataTypes

--eval :: Env -> LispVal -> ThrowsError (LispVal, Env)
--eval env val@(ValString _) = return (val, env)
--eval env val@(ValBool _) = return (val, env)
--eval env val@(ValNum _) = return (val, env)
--eval env (Atom ident) = evalAtom env ident
--eval env (ValList [Atom "quote", val]) = return (val, env)
--eval env (ValList (Atom "define" : args)) = define env args
--eval env (ValList (Atom "cond" : args)) = cond env args
--eval env (ValList (Atom "if" : args)) = ifStatement env args
--eval env (ValList (Atom "let" : args)) = letStatement env args
--eval env (ValList (Atom "lambda" : args)) = lambda env args
--eval env (ValList (func : args)) = evalFunc env (ValList (func : args))
--eval _ syntaxError = throw $ KeywordError syntaxError
--interpretLispExpr  env (Cons (Atom ident) body) = compSpe env ident body
--interpretLispExpr  env (Cons fun rest) = FunCall <$> interpretLispExpr env fun <*> toList env rest
--
---- | -----------------------------------------------------------------------------------------------------------------
--evalFunc :: Env -> LispVal -> ThrowsError (LispVal, Env)
--evalFunc env (ValList (headExpr : argExprs)) = do
--  (result, _) <- eval env headExpr
--  args <- evalArgs [] env argExprs
--  evalFunc' env result args
--evalFunc _ (Atom "#f") = throw . NotFunction $ Atom "#f"
--evalFunc _ (Atom "#t") = throw . NotFunction $ Atom "#t"
--evalFunc _ notFunc = throw $ NotFunction notFunc
--
---- | -----------------------------------------------------------------------------------------------------------------
---- Ignoring any new environment, since Lisp expressions cannot affect name bindings at a higher-level scope,
---- apart from define, which is valid only as a top-level expression.
--evalFunc' :: Env -> LispVal -> [LispVal] -> ThrowsError (LispVal, Env)
--evalFunc' env (Func closure (LispFct f)) args = do
--  result <- f (mergeEnvs env closure) args
--  return (result, env)
--evalFunc' _ notFunc _ = throw $ NotFunction notFunc
--
---- | -----------------------------------------------------------------------------------------------------------------
---- Old evaluation function, kept for refactoring later
--apply :: String -> Env -> [LispVal] -> ThrowsError (LispVal, Env)
--apply func env args = case lookup func builtins of
--  Just f -> do
--    res <- f args
--    return (res, env)
--  Nothing -> throw . NotFunction $ Atom func
--
---- | -----------------------------------------------------------------------------------------------------------------
---- Get Variable from env (Value or Func) or search in builtins
--evalAtom :: Env -> Identifier -> ThrowsError (LispVal, Env)
--evalAtom env "#f" = return (Atom "#f", env)
--evalAtom env "#t" = return (Atom "#t", env)
--evalAtom env ident = getEnvVar env ident <|> getBuiltins env ident
--
---- TODO: refacto
--evalArgs :: [LispVal] -> Env -> [LispVal] -> ThrowsError [LispVal]
--evalArgs acc _ [] = return acc
--evalArgs acc env (first : left) = do
--  (result, _) <- eval env first
--  evalArgs (acc ++ [result]) env left
--
---- | -----------------------------------------------------------------------------------------------------------------
----  Allows to conditionally evaluate expressions. It takes a variable number of arguments.
----  Each argument is a list. “cond” successively evaluates the first element of each list.
----  If its return value is true, it evaluates the second element of the list and returns it’s value.
----  Otherwise, it tries the next expression.
----
----  Syntax: (cond <expr> <expr> ...)
---- TODO: getNewEnv for each eval ? (nested envs ?)
--cond :: Env -> [LispVal] -> ThrowsError (LispVal, Env)
--cond env [] = return (ValBool False, env)
--cond env (ValList [condition, validated] : other) = do
--  (resEval, _) <- eval env condition
--  res <- isTrueExpr resEval
--  (if res then eval env validated else cond env other)
---- TODO: change Error ?
--cond _ a = throw $ SyntaxError "Error in cond"
--
---- | -----------------------------------------------------------------------------------------------------------------
----  define keyword used for creating global variable name bindings. This
----  operation may overwrite existing bindings, if present.
----
----  Syntax: (define <name> <expr>)
--define :: Env -> [LispVal] -> ThrowsError (LispVal, Env)
--define _ [] = throw $ NbArgsError "define" 2 []
--define _ [a] = throw $ NbArgsError "define" 2 [a]
--define env [Atom name, expr] = do
--  (result, _) <- eval env expr
--  return (Atom name, addEnvVar env name result)
--define env [ValList (Atom name : args), body] = return (Atom name, addEnvVar env name (Func env (LispFct fct)))
--  where
--    fct internEnv callArgs
--      | length callArgs /= length args = throw $ NbArgsError name (genericLength args) callArgs
--      | otherwise = do
--        argNames <- getArgNames [] args
--        (res, _) <- eval (composeEnv argNames) body
--        return res
--      where
--        composeEnv argNames = addVarsToEnv internEnv (zip argNames callArgs)
--define _ [_, _] = throw $ SyntaxError "define (define <name> <expr>)"
--define _ args = throw $ NbArgsError "define" 2 args
--
---- TODO: refacto
--getArgNames :: [Identifier] -> [LispVal] -> ThrowsError [Identifier]
--getArgNames acc [] = return acc
--getArgNames acc ((Atom ident) : exprs) = getArgNames (acc ++ [ident]) exprs
--getArgNames _ (expr : _) = throw . SyntaxError $ "Invalid argument identifier `" ++ show expr ++ "` in lambda expression"
--
---- | -----------------------------------------------------------------------------------------------------------------
----  lambda keyword for creating anonymous functions within Lisp code. This
----  includes an (admittedly rudimentary) implementation of closures, in the sense
----  that the environment in which an anonymous function is created is stored
----  in the function returned by lambda.
----
----  Syntax: (lambda (<arg-ids> ...) <expr>)
--lambda :: Env -> [LispVal] -> ThrowsError (LispVal, Env)
--lambda _ [] = throw $ NbArgsError "lambda" 2 []
--lambda _ [a] = throw $ NbArgsError "lambda" 2 [a]
--lambda env [ValList argExprs, expr] = do
--  argNames <- getArgNames [] argExprs
--  return (Func env $ LispFct $ lambda' env argNames expr, env)
--lambda _ [_, _] = throw $ SyntaxError "(lambda (<arg-ids> ...) <expr>"
--lambda _ args = throw $ NbArgsError "lambda" 2 args
--
--lambda' :: Env -> [Identifier] -> LispVal -> (Env -> [LispVal] -> ThrowsError LispVal)
--lambda' env argNames expr callerEnv args
--  | length argNames /= length args = throw $ NbArgsError "#<procedure>" (genericLength args) args
--  | otherwise = do
--    (result, _) <- eval internEnv expr
--    return result
--  where
--    internEnv = mergeEnvs (addVarsToEnv env (zip argNames args)) callerEnv
--
---- | -----------------------------------------------------------------------------------------------------------------
---- if statement that evaluates the first argument. If the condition evaluates
---- to true, then the then-expression is evaluated and its result returned,
---- otherwise the else-expression is evaluated and its result is returned.
----
---- Syntax: (if <cond> <then-expr> <else-expr>)
--ifStatement :: Env -> [LispVal] -> ThrowsError (LispVal, Env)
--ifStatement env [condition, validated, other] = do
--  condEvaluated <- eval env condition
--  case condEvaluated of
--    (ValBool True, newEnv) -> eval newEnv validated
--    _ -> eval env other
--ifStatement _ a = throw $ NbArgsError "if" 3 a
--
---- | -----------------------------------------------------------------------------------------------------------------
---- let statement. Evaluates the given expression with the specified name
---- bindings, possibly shadowing any previously-bound variable names.
---- Syntax: (let ((<name> <value>) ...) <expr>)
--letStatement :: Env -> [LispVal] -> ThrowsError (LispVal, Env)
--letStatement _ [] = throw $ NbArgsError "let" 2 []
--letStatement _ [a] = throw $ NbArgsError "let" 2 [a]
--letStatement env [bindingsList, expr] = do
--  bindings <- parseLetBindings env bindingsList
--  (result, _) <- eval (mergeEnvs bindings env) expr
--  return (result, env)
--letStatement _ args = throw $ NbArgsError "let" 2 args
--
--mapBinding :: Env -> LispVal -> ThrowsError (Identifier, LispVal)
--mapBinding env (ValList [Atom name, expr]) = do
--  (result, _) <- eval env expr
--  return (name, result)
--mapBinding _ _ = throw $ SyntaxError "Malformed let expression."
--
--parseLetBindings :: Env -> LispVal -> ThrowsError Env
--parseLetBindings env (ValList listExpr) = addVarsToEnv env <$> mapM (mapBinding env) listExpr
--parseLetBindings _ _ = throw $ SyntaxError "Malformed let expression."
--
---- | -----------------------------------------------------------------------------------------------------------------
---- Boolean helpers used in cond to not define this at top-level
--isTrueExpr :: LispVal -> ThrowsError Bool
--isTrueExpr (ValBool True) = return True
--isTrueExpr (Atom "#t") = return True
--isTrueExpr (ValList []) = return False
--isTrueExpr (ValNum 0) = return False
--isTrueExpr (ValNum _) = return True
--isTrueExpr (ValList _) = return True
--isTrueExpr a = return False

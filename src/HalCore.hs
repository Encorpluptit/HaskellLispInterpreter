module HalCore
  ( halCore,
  )
where

import FileManagement
import HalDataTypes
import HalEnvironment
import HalError
import HalOptions
import HalREPL
import LispEvaluation
import LispExpression
import PrintUtils

-- | -----------------------------------------------------------------------------------------------------------------
-- Core function:
--  * Process file if needed
--  * Launch Repl if repl option in Opts.
--  * Print AST or Value if showTree option in Opts (Stored in printFct).
halCore :: Opts -> [String] -> IO ()
halCore opts@(Opts replOpt _ _) files
  | replOpt = evalFiles >>= launchRepl opts . fst
  | otherwise = evalFiles >> return ()
  where
    evalFiles = processFiles opts files (emptyEnv, Nothing)

processFiles :: Opts -> [String] -> (Env, Maybe HalExpr) -> IO (Env, Maybe HalExpr)
processFiles _ [] res = return res
processFiles opts (file : left) result =
  loadFile file >>= getExpr >>= evalExprList
  where
    getExpr = getExprListFromContent opts
    evalExprList content = evalContent opts result content >>= processFiles opts left

getExprListFromContent :: Opts -> String -> IO [LispExpr]
getExprListFromContent (Opts _ printAst debugMode) str = case unpackError $ parseContent str of
  Left err -> writeErrorAndExit err
  Right a -> if debugMode then printThis a else return a
    where
      printThis res = mapM_ (printLispExpr printAst putStrLn) res >> return res

evalContent :: Opts -> (Env, Maybe HalExpr) -> [LispExpr] -> IO (Env, Maybe HalExpr)
evalContent (Opts _ _ False) (env, precExpr) exprs =
  case unpackError $ evalLispExprList env exprs of
    Right (newEnv, Nothing) -> return (newEnv, precExpr)
    Right (newEnv, newExpr) -> return (newEnv, newExpr)
    Left err -> writeErrorAndExit err
evalContent (Opts _ printAst True) (env, precExpr) exprs =
  case unpackError $ evalLispExprList env exprs of
    Right (newEnv, Nothing) -> putStrLn "Nothing" >> return (newEnv, precExpr)
    Right res@(_, Just newExpr) -> printThis newExpr >> return res
    Left err -> writeErrorAndExit err
  where
    printThis = printHalExpr printAst putStrLn
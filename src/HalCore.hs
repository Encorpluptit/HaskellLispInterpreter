module HalCore
  ( halCore,
  )
where

import FileManagement
import HalDataTypes
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
halCore opts@(Opts replOpt _) files
  | replOpt = evalFiles >>= launchRepl opts . fst
  | otherwise = evalFiles >>= printHalExpr opts
  where
    evalFiles = processFiles opts emptyEnv Nothing files


processFiles :: Opts -> Env -> Maybe HalExpr -> [String] -> IO (Env, Maybe HalExpr)
processFiles _ env oldExpr [] = return (env, oldExpr)
processFiles opts env oldExpr (file : left) =
  loadFile file >>= evalFileContent
  where
      evalFileContent content =  case unpackError $ evalContent opts env content of
          Right (newEnv, Nothing) -> processFiles opts newEnv oldExpr left
          Right (newEnv, newExpr) -> processFiles opts newEnv newExpr left
          Left err -> writeErrorAndExit err

printHalExpr :: Opts -> (Env, Maybe HalExpr) -> IO ()
printHalExpr (Opts True _) _ = return ()
printHalExpr (Opts _ _) (_, Nothing) = return ()
printHalExpr (Opts _ False) (_, Just expr) = putStrLn $ showHalExpr expr
printHalExpr (Opts _ True) (_, Just expr) = print expr

evalContent :: Opts -> Env -> String -> ThrowsHalExprError (Env, Maybe HalExpr)
evalContent (Opts _ False) env str = case parseContent str of
      Right a -> evalLispExprList env a
      Left err -> throw $ FileError err
evalContent (Opts _ True) env str = case parseContent str of
      Right a ->  evalLispExprList env a
      Left err -> throw $ FileError err

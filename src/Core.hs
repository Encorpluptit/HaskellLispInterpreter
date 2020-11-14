module Core
  ( halCore,
  )
where

import Control.Monad (void)
import DataTypes
import Environment
import Errors
import File
import LibParsing
import Options
import Parser
import PrintUtils
import REPL

-- | -----------------------------------------------------------------------------------------------------------------
-- Core function:
--  * Process file if needed
--  * Launch Repl if repl option in Opts.
--  * Print AST or Value if showTree option in Opts (Stored in printFct).
halCore :: Opts -> [String] -> IO ()
halCore opts@(Opts replOpt _) files
  | replOpt = manageFiles >>= launchRepl printFct
  | otherwise = Control.Monad.void manageFiles
  where
    manageFiles = processFiles printFct files emptyEnv
    printFct = getPrintFct opts

-- | -----------------------------------------------------------------------------------------------------------------
-- Process file list given in program arguments:
--  * Call getArgsFiles that read files
--  * print with .
--  * Print AST or Value with showTree option in Opts.
-- TODO: remove print and process in halCore ???
processFiles :: (Env -> String -> IO Env) -> [String] -> Env -> IO Env
--processFiles printFct files = do
--  processedFiles <- getArgsFiles files
--  mapM_ printFct processedFiles
-- TODO [MARC]: Ask why this doesn't work ?
processFiles _ [] env = return env
processFiles printFct (x : xs) env = do
  file <- loadFile x
  printFct env file >>= processFiles printFct xs

-- | -----------------------------------------------------------------------------------------------------------------
-- Get print function from Opts:
--  * showTree == False -> print value
--  * showTree == True -> print Abstract Syntax Tree
getPrintFct :: Opts -> (Env -> String -> IO Env)
getPrintFct (Opts _ False) = printValue
getPrintFct (Opts _ True) = printAST

printValue :: Env -> String -> IO Env
printValue env s = case unpackError $ parseExpr env s of
  Right (x, newEnv) -> (putStrLn . showVal) x >> return newEnv
--  Left err -> writeErrorAndExit err
  Left err -> writeError err >> return env

printAST :: Env -> String -> IO Env
printAST env s = case runParser parseLispVal s of
  Right (x, _) -> print x >> return env
  Left err -> writeError err >> return env

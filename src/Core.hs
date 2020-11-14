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
  | replOpt = manageFiles >>= launchRepl printer
  | otherwise = Control.Monad.void manageFiles
  where
    manageFiles = processFiles filePrinter files emptyEnv
    filePrinter = getPrintFct opts{repl=False}
    printer = getPrintFct opts

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
getPrintFct (Opts replOpt False) = printValue replOpt
getPrintFct (Opts replOpt True) = printAST replOpt

printValue :: Bool -> Env -> String -> IO Env
printValue replOpt env s = case unpackError $ parseExpr env s of
  Right (x, newEnv) -> (putStrLn . showVal) x >> return newEnv
  Left err ->
    ( if replOpt
        then writeError err >> return env
        else writeErrorAndExit err
    )

printAST :: Bool -> Env -> String -> IO Env
printAST replOpt env s = case runParser parseLispVal s of
  Right (x, _) -> print x >> return env
  Left err ->
    ( if replOpt
        then writeError err >> return env
        else writeErrorAndExit err
    )

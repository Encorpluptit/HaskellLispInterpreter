module Core
  ( halCore,
  )
where

import LibParsing
import Errors
import File
import DataTypes
import Options
import Parser
import REPL

-- | -----------------------------------------------------------------------------------------------------------------
-- Core function:
--  * Process file if needed
--  * Launch Repl if repl option in Opts.
--  * Print AST or Value if showTree option in Opts (Stored in printFct).
halCore :: Opts -> [String] -> IO ()
halCore opts@(Opts replOpt _) files
    | replOpt = manageFiles >> launchRepl printFct
    | otherwise = manageFiles
        where
            manageFiles = processFiles printFct files
            printFct = getPrintFct opts

-- | -----------------------------------------------------------------------------------------------------------------
-- Process file list given in program arguments:
--  * Call getArgsFiles that read files
--  * print with .
--  * Print AST or Value with showTree option in Opts.
-- TODO: remove print and process in halCore ???
processFiles :: (String -> String) -> [String] -> IO ()
processFiles printFct files = do
  processedFiles <- getArgsFiles files
  mapM_ (putStrLn . printFct) processedFiles
-- TODO [MARC]: Ask why this doesn't work ?
--    (x:xs) <- getArgsFiles files
--    putStrLn (process x) >> processFiles xs

-- | -----------------------------------------------------------------------------------------------------------------
-- Get print function from Opts:
--  * showTree == False -> print value
--  * showTree == True -> print Abstract Syntax Tree
getPrintFct :: Opts -> (String -> String)
getPrintFct (Opts _ False) = printValue
getPrintFct (Opts _ True) = printAST

printValue :: String -> String
printValue s = case unpackError $ parseExpr s of
  Right x -> showVal x
  Left err -> show err

printAST :: String -> String
printAST s = case runParser parseLispVal s of
  Right x -> show x
  Left err -> show err

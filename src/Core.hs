module Core
  ( halCore,
  )
where

import LibParsing
import Errors
import File
import DataTypes
import Options
import PrintUtils
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
processFiles :: (String -> IO ()) -> [String] -> IO ()
--processFiles printFct files = do
--  processedFiles <- getArgsFiles files
--  mapM_ printFct processedFiles
-- TODO [MARC]: Ask why this doesn't work ?
processFiles _ [] = return ()
processFiles printFct (x:xs) = do
    file <- loadFile x
    printFct file >> processFiles printFct xs
--processFiles _ [] = return ()
--processFiles printFct files = do
--    (x:xs) <- getArgsFiles files
--    printFct x >> processFiles printFct xs

-- | -----------------------------------------------------------------------------------------------------------------
-- Get print function from Opts:
--  * showTree == False -> print value
--  * showTree == True -> print Abstract Syntax Tree
getPrintFct :: Opts -> (String -> IO ())
getPrintFct (Opts _ False) = printValue
getPrintFct (Opts _ True) = printAST

printValue :: String -> IO ()
printValue s = case unpackError $ parseExpr s of
  Right x -> (putStrLn . showVal) x
  Left err -> writeErrorAndExit err

printAST :: String -> IO ()
printAST s = case runParser parseLispVal s of
  Right x -> print x
  Left err -> writeErrorAndExit err

module Core
  ( halCore,
  )
where

import Control.Monad (void)
import DataTypes
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
  | replOpt = manageFiles >>= launchRepl (printer True)
  | otherwise = Control.Monad.void manageFiles
  where
    manageFiles = processFiles filePrinter files emptyEnv
    filePrinter = getPrintFct opts {repl = False}
    printer = getPrintFct opts

-- | -----------------------------------------------------------------------------------------------------------------
-- Process file list given in program arguments:
--  * Call getArgsFiles that read files
--  * print with .
--  * Print AST or Value with showTree option in Opts.
-- TODO: remove print and process in halCore ???
processFiles :: (Bool -> Env -> String -> IO Env) -> [String] -> Env -> IO Env
processFiles _ [] env = return env
--processFiles printFct (x : xs) env = loadFile x >>= printFct env >>= processFiles printFct xs
processFiles printFct [x] env = do
  file <- loadFile x
  parseEntireFile printFct (lines file) env
processFiles printFct (x : xs) env = do
  file <- loadFile x
  parseEntireFile printFct (lines file) env >>= processFiles printFct xs

parseEntireFile :: (Bool -> Env -> String -> IO Env) -> [String] -> Env -> IO Env
parseEntireFile _ [] _ = writeErrorAndExit "Empty File."
parseEntireFile printFct [x] env = printFct True env x
parseEntireFile printFct (x : y : xs) env = case runParser parseLispVal x of
  Left _ -> parseEntireFile printFct ((x ++ y) : xs) env
  Right (_, []) -> case all isSep y of
    True -> case (xs) of
      [] -> printFct True env x
      _ -> printFct False env x >>= parseEntireFile printFct (xs)
    False -> printFct False env x >>= parseEntireFile printFct (y : xs)
  Right (_, rest) -> writeErrorAndExit ("MDR T MOVAIS" ++ show rest)

-- | -----------------------------------------------------------------------------------------------------------------
-- Get print function from Opts:
--  * showTree == False -> print value
--  * showTree == True -> print Abstract Syntax Tree
getPrintFct :: Opts -> (Bool -> Env -> String -> IO Env)
getPrintFct (Opts replOpt False) = printValue replOpt
getPrintFct (Opts replOpt True) = printAST replOpt

printValue :: Bool -> Bool -> Env -> String -> IO Env
printValue replOpt printResult env s = case unpackError $ parseExpr env s of
  Right (x, newEnv) ->
    ( if printResult
        then (putStrLn . showVal) x >> return newEnv
        else return newEnv
    )
  Left err ->
    ( if replOpt
        then writeError err >> return env
        else writeErrorAndExit err
    )

printAST :: Bool -> Bool -> Env -> String -> IO Env
printAST replOpt printResult env s = case runParser parseLispVal s of
  Right (x, _) -> print x >> return env
  Left err ->
    ( if replOpt
        then writeError err >> return env
        else writeErrorAndExit err
    )

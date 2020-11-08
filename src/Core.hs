module Core where

import Errors
import File
import LibParsing
import Parser

--import PrintUtils

--import REPL
--import Arguments

processFiles :: [String] -> IO ()
processFiles files = do
  processedFiles <- getArgsFiles files
  mapM_ (putStrLn . process) processedFiles

--    (x:xs) <- getArgsFiles files
--    putStrLn (process x) >> processFiles xs

process :: String -> String
process s = case unpackError $ parseExpr s of
  Right x -> show x
  Left err -> show err

run :: Parser a -> String -> Result a
run (Parser p) = p

--run (Parser p) str = case p str of
--  Right (a, []) -> Right (a, [])
--  Left msg -> Left msg

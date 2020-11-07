module Core where

import PrintUtils
import LibParsing
import Errors
import Parser
--import REPL
--import Arguments

process :: String -> IO ()
process s = case unpackError $ parseExpr s of
  Right x -> printAndExit x
  Left err -> writeErrorAndExit err

run :: Parser a -> String -> Result a
run (Parser p) str = case p str of
  Right (a, []) -> Right (a, [])
  Left msg -> Left msg

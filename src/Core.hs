module Core where

import Errors
import LibParsing
import Parser
import PrintUtils

--import REPL
--import Arguments

process :: String -> String
process s = case unpackError $ parseExpr s of
  Right x -> show x
  Left err -> show err

run :: Parser a -> String -> Result a
run (Parser p) str = case p str of
  Right (a, []) -> Right (a, [])
  Left msg -> Left msg

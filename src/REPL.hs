module REPL where

import Control.Applicative
import Data.List (isPrefixOf)
import Environment
import Parser
import System.Console.Haskeline
import Core (process)

launchRepl :: IO ()
launchRepl = runInputT replSettings loop

loop :: InputT IO ()
loop = do
  line <- getInputLine "|λ〉"
  case line of
    Nothing -> outputStrLn "Crtl + D Pressed !"
    Just "quit" -> outputStrLn "Bye."
    Just input -> do outputStrLn (process input) >> loop

-- | -----------------------------------------------------------------------------------------------------------------
-- Haskeline Settings:
replSettings :: Settings IO
replSettings =
  Settings
    { --    complete = completeFilename,
      complete = halAutoComplete,
      historyFile = Just ".history",
      autoAddHistory = True
    }

-- | -----------------------------------------------------------------------------------------------------------------
-- Auto Completion:
halAutoComplete :: CompletionFunc IO
halAutoComplete = completeWord Nothing "( \t" $ return . searchFunc

--halAutoComplete = keywords
--    where keywords = completeWord Nothing "( \t" (return . searchFunc) <|> completeFilename

searchFunc :: String -> [Completion]
searchFunc str = map simpleCompletion $ filter (str `isPrefixOf`) halKeyWords

halKeyWords :: [String]
halKeyWords =
  [ "define",
    "lambda",
    "remainder",
    "quotient",
    "number?",
    "bool?",
    "list?",
    "string?",
    "symbol?",
    "||",
    "&&",
    "string=?",
    "string>?",
    "string<?",
    "string<=?",
    "string>=?",
    "string-length",
    "atom?",
    "car",
    "cdr",
    "con",
    "cond",
    "eq?"
  ]

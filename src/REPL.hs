module REPL where

import Data.List (isPrefixOf)
import System.Console.Haskeline
import Control.Monad.IO.Class
import Environment
import Debug.Trace

-- | -----------------------------------------------------------------------------------------------------------------
-- Haskeline REPL:
--  * launchRepl: Init lopp with default settings
--  * loop: Core of REPL fct.
launchRepl :: (Env -> String -> IO Env) -> Env -> IO ()
launchRepl printFct env = runInputT replSettings $ loop printFct env

loop :: (Env -> String -> IO Env) -> Env -> InputT IO ()
loop printFct env = do
  line <- getInputLine "|λ〉"
  case line of
    Nothing -> outputStrLn "Crtl + D Pressed !"
    Just "quit" -> outputStrLn "Bye."
--    Just input -> liftIO (printFct env input) >>= loop printFct
    Just input -> do
        newEnv <- liftIO (printFct env input)
        trace (show newEnv) loop printFct newEnv

--        liftIO (printFct input) >> loop printFct

-- | -----------------------------------------------------------------------------------------------------------------
-- Haskeline Settings:
--  * complete: auto-completion function
--  * historyFile: Maybe String with the filepath to history file (up and down Arrow to move between history lines)
--  * autoAddHistory: Auto add each line returned byt getInputLine
replSettings :: Settings IO
replSettings =
  Settings
    { --    complete = completeFilename,
      complete = halAutoComplete,
      historyFile = Just ".history",
      autoAddHistory = True
    }

-- | -----------------------------------------------------------------------------------------------------------------
-- Hal Auto Completion Function (based on Haskeline tools):
--  * fallbackCompletion:
--      If the first completer produces no suggestions, fallback to the second completer's output.
--  * completeFilename:
--      A completion command for file and folder names.
--  * completeScheme:
--      Search in Scheme list of keywords possible completion.
--  * completeWord :: Monad m => Maybe Char -> [Char] -> (String -> m [Completion]) -> CompletionFunc m
--      - Maybe Char: An optional escape character.
--      - [Char]: Characters which count as whitespace.
--      - (String -> m [Completion]): Function to produce a list of possible completions.
halAutoComplete :: CompletionFunc IO
halAutoComplete = fallbackCompletion completeScheme completeFilename
    where completeScheme = completeWord Nothing "( \t" $ return . searchSchemeKeywords

-- | -----------------------------------------------------------------------------------------------------------------
-- Construct a list of possible Completion if input string is prefix of schemeKeyWords.
searchSchemeKeywords :: String -> [Completion]
searchSchemeKeywords str = map simpleCompletion $ filter (str `isPrefixOf`) schemeKeyWords

-- | -----------------------------------------------------------------------------------------------------------------
-- List Scheme keywords handled in HAL.
schemeKeyWords :: [String]
schemeKeyWords =
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

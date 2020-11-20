module HalREPL where

import Control.Monad.IO.Class
import Data.List (isPrefixOf)
import System.Console.Haskeline
import HalOptions
import LispEvaluation
import HalDataTypes

-- | -----------------------------------------------------------------------------------------------------------------
-- Haskeline REPL:
--  * launchRepl: Init lopp with default settings
--  * loop: Core of REPL fct.
--launchRepl :: (Env -> String -> IO Env) -> Env -> IO ()
launchRepl :: Opts -> Env -> IO ()
launchRepl opts env = runInputT replSettings $ loop opts env

loop :: Opts -> Env -> InputT IO ()
loop opts env = getInputLine "|λ〉" >>= inputHandler
  where
    inputHandler Nothing = outputStrLn "Crtl + D Pressed !"
    inputHandler (Just "quit") = outputStrLn "Bye."
    inputHandler (Just "(debug)") = outputStrLn "Debug Mode !" >> loop opts{debug = not $ printAST opts} env
    inputHandler (Just "(print-ast)") = outputStrLn "Showing Tree !" >> loop opts{printAST = not $ printAST opts} env
--    inputHandler (Just input) = outputStrLn input >> loop opts env
    inputHandler (Just input) = outputStrLn input >> loop opts env



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
  where
    completeScheme = completeWord Nothing "( \t" $ return . searchSchemeKeywords

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

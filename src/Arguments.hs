module Arguments where

import PrintUtils
import System.Console.GetOpt
import System.Environment
import System.Exit

newtype Opts = Opts {repl :: Bool}
  deriving (Show)

defaultOpts :: Opts
defaultOpts =
  Opts
    { repl = False
    }

-- | -----------------------------------------------------------------------------------------------------------------
-- Using System.Console.GetOpt to describe flags with associated description and actions to perform.
--  * OptDescr a = Option [Char] [String] (ArgDescr a) String
--      - Option [Char] -> list of short option characters
--      - [String]      -> list of long option strings (without "--")
--      - (ArgDescr a)  -> argument descriptor
--          - NoArg                     -> no argument expected
--          - ReqArg (String -> a)      -> option requires argument
--          - OptArg (Maybe String -> a)-> optional argument
--      - String        -> explanation of option for user
--  * usageInfo: Return a string describing the usage of a command
flags :: [OptDescr (Opts -> IO Opts)]
flags =
  [ Option
      ['i']
      ["interactive"]
      (NoArg $ \opt -> return opt {repl = True})
      "Start REPL (Read -> Eval -> Print -> Loop) after evaluation of other args (IE: input files)",
    Option
      ['v']
      ["version"]
      -- TODO: regexp for version
      (NoArg $ const $ printAndExit "Version ")
      "Display Hal version",
    Option
      ['h']
      ["help"]
      ( NoArg $ \_ -> do
          prg <- getProgName
--          printAndExit $ usageInfo prg flags
          putStrLn (usageInfo prg flags) >> exitSuccess
      )
      "Show this help text"
  ]

-- | -----------------------------------------------------------------------------------------------------------------
-- Using System.Console.GetOpt to parse program arguments.
--  * fileNames: Unparsed Arguments.
--  * errors: Errors raised by getOpt when parsing a non-existent short or long flag.
--  * actions: TODO, not quite understand yet
manageArgs :: [String] -> IO (Opts, [String], [String])
manageArgs [] = writeErrorAndExit "Need at least one argument !"
manageArgs args = do
  opts <- foldl (>>=) (return defaultOpts) actions
  return (opts, fileNames, errors)
  where
    (actions, fileNames, errors) = getOpt RequireOrder flags args

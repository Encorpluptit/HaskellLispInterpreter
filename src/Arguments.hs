module Arguments where

import Options
import PrintUtils
import System.Console.GetOpt
import System.Environment
import System.Exit

-- | -----------------------------------------------------------------------------------------------------------------
-- Using System.Console.GetOpt to parse program arguments.
--  * Resources:
--      - https://wiki.haskell.org/High-level_option_handling_with_GetOpt
--      - https://hackage.haskell.org/package/base-4.14.0.0/docs/System-Console-GetOpt.html
--  * fileNames: Unparsed Arguments.
--  * errors: Errors raised by getOpt when parsing a non-existent short or long flag.
--  * actions: TODO, not quite understand yet
manageArgs :: [String] -> IO (Opts, [String], [String])
manageArgs [] = writeErrorAndExit "Need at least one argument !"
manageArgs args = do
  opts <- foldl (>>=) (return defaultOpts) actions
  return (opts, fileNames, errors)
  where
    --    (actions, fileNames, errors) = getOpt RequireOrder flags args
    (actions, fileNames, errors) = getOpt Permute flags args

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
--  * usageInfo: Return a string describing the usage of all managed flags.
--  TODO: Manage Output as AST or finished product
flags :: [OptDescr (Opts -> IO Opts)]
flags =
  [ Option
      ['i']
      ["interactive"]
      (NoArg $ \opt -> return opt {repl = True})
      "Start REPL (Read -> Eval -> Print -> Loop) after evaluation of other args (IE: input files)",
    Option
      ['s']
      ["show-tree"]
      (NoArg $ \opt -> return opt {showTree = True})
      "[DEBUG]: Showing AST instead of result.",
    Option
      ['v']
      ["version"]
      -- TODO: regexp for version (possible as not sure if IO ? (but IO Opts, should be good))
      (NoArg $ const $ printAndExit "Version: Alpha")
      "Display Hal version",
    Option
      ['h']
      ["help"]
      ( NoArg $ \_ -> do
          prg <- getProgName
          --          printAndExit $ usageInfo prg flags
          putStrLn (usageInfo prg flags) >> exitSuccess
      )
      "Show this help."
  ]

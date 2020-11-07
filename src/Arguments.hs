module Arguments where

import System.Environment
import System.IO
import System.Exit
import System.Console.GetOpt
import ExitError

newtype Opts = Opts {repl :: Bool}
    deriving (Show)

defaultOpts :: Opts
defaultOpts = Opts {
    repl = False
}

-- |Option descriptions for command line flags.
flags :: [OptDescr (Opts -> IO Opts)]
flags =
    [ Option ['i'] ["interactive"]
        (NoArg $ \opt -> return opt { repl = True }) $
        "Evaluate contents of supplied files, and initialize interactive "
            ++ "read-eval-print-loop"
    , Option ['v'] ["version"]
        (NoArg $ \_ -> do
            hPutStrLn stderr "Version "
            exitSuccess)
        "Display hasp version info"
    , Option ['h'] ["help"]
        (NoArg $ \_ -> do
            prg <- getProgName
            hPutStrLn stderr $ usageInfo prg flags
            exitSuccess)
        "Show this help text" ]

manageArgs :: [String] -> IO (Opts, [String], [String])
manageArgs [] = writeErrorAndExit "Need at least one args"
manageArgs args = do
    opts <- foldl (>>=) (return defaultOpts) actions
    return (opts, fileNames, errors)
        where
            (actions, fileNames, errors) = getOpt RequireOrder flags args


parseArgs :: [String] -> Opts -> IO Opts
parseArgs [] opts = return opts
parseArgs ("-i":_) opts = return newOpts
    where newOpts = Opts {repl = True}

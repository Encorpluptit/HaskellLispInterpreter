module IOUtils where

import System.Exit
import System.IO

writeError :: String -> IO ()
writeError str = hPutStrLn stderr ("Error: " ++ str)

exitError :: String -> IO a
exitError [] = exitWith $ ExitFailure 84
exitError str = do
    writeError str
    exitWith  $ ExitFailure 84

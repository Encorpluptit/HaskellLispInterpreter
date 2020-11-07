module ExitError where

import System.Exit
import System.IO

writeError :: Show a => a -> IO ()
writeError str = hPutStrLn stderr ("Error: " ++ show str)

exitError :: IO a
exitError = exitWith $ ExitFailure 84

writeErrorAndExit :: Show a => a -> IO b
writeErrorAndExit str = (writeError . show) str >> exitError

writeErrorsAndExit :: Show a => [a] -> IO a
writeErrorsAndExit = foldr ((>>) . writeError) exitError
--writeErrorsAndExit :: [String] -> IO a
--writeErrorsAndExit = foldr ((>>) . writeError) exitError
--writeErrorsAndExit [] = exitWith  $ ExitFailure 84
--writeErrorsAndExit (x:xs) = writeError x >> writeErrorsAndExit xs

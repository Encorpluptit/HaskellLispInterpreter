module PrintUtils where

import System.Exit
import System.IO

exitError :: IO a
exitError = exitWith $ ExitFailure 84

writeError :: Show a => a -> IO ()
writeError str = hPutStrLn stderr ("Error: " ++ show str)

writeErrorAndExit :: Show a => a -> IO b
writeErrorAndExit str = (writeError . show) str >> exitError

writeErrorsAndExit :: Show a => [a] -> IO b
writeErrorsAndExit args = mapM_ writeError args >> exitError
--writeErrorsAndExit = foldr ((>>) . writeError) exitError

-- | Equivalent: TODO [MARC]: Ask which better way ?
-- writeErrorsAndExit args = mapM_ writeError args >> exitError

--writeErrorsAndExit :: [String] -> IO a
--writeErrorsAndExit = foldr ((>>) . writeError) exitError
--writeErrorsAndExit [] = exitWith  $ ExitFailure 84
--writeErrorsAndExit (x:xs) = writeError x >> writeErrorsAndExit xs

printAndExit :: Show a => a -> IO b
printAndExit msg = print msg >> exitSuccess

printListAndExit :: Show a => [a] -> IO b
printListAndExit msg = mapM_ print msg >> exitSuccess

printStrAndExit :: String -> IO b
printStrAndExit msg = putStrLn msg >> exitSuccess

printListStrAndExit :: [String] -> IO b
printListStrAndExit msg = mapM_ printStrAndExit msg >> exitSuccess

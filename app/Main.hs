module Main where

import System.Environment
import PrintUtils
import Core
import Arguments
import REPL

main :: IO ()
main = do
    args <- getArgs
    res <- manageArgs args
    case res of
        (_, _, err@(_:_))       -> writeErrorsAndExit err
--        (Opts False, files, _)  -> printListAndExit files
        (Opts False, files, _)  -> processFiles files
        (Opts True, _, _)       -> launchRepl
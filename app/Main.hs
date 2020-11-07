module Main where

import System.Environment
import PrintUtils
import Arguments
import REPL

main :: IO ()
main = do
    args <- getArgs
    res <- manageArgs args
    case res of
        (_, _, err@(_:_)) -> writeErrorsAndExit err
        (Opts False, _, _) -> launchRepl
        (Opts True, _, _) -> launchRepl
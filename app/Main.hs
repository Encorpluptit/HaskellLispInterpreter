module Main where

import System.Environment
import PrintUtils
import HalCore
import HalOptions

main :: IO ()
main = do
    args <- getArgs
    res <- manageArgs args
    case res of
        (_, _, err@(_:_))   -> writeErrorsAndExit err
        (opts, files, _)    -> halCore opts files

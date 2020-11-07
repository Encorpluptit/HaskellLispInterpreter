module Main where

import System.Environment
import LibParsing
import ExitError
import REPL
import Arguments
import System.Console.GetOpt

--main :: IO ()
--main = do
--    someFunc
--    case runParser parseInt "-42a" of
--        Left a  -> putStrLn a
--        Right b -> print b



main :: IO ()
main = do
    args <- getArgs
    res <- manageArgs args
    case res of
        (_, _, err@(_:_)) -> writeErrorsAndExit err
--        ()
--    let (actions, fileNames, errors) = getOpt RequireOrder flags args
--    opts <- foldl (>>=) (return defaultOpts) actions
--    print opts
--    print fileNames
--    print errors

--    action args
----    opts <- foldl (>>=) (return defaultOptions) actions
--    _ <- foldl (>>=) (return defaultOpts) actions
--    print "ok"
----    print $ getOpt RequireOrder flags args
--    where
--        action args = getOpt RequireOrder flags args
--        action args = let (actions, fileNames, errors) = getOpt RequireOrder flags args
--                in do
--                    print fileNames
--                    print errors
----                    return actions
--    where
--        (actions, fileNames, errors) = getOpt RequireOrder flags args
--    launchRepl

--main :: IO ()
--main = launchRepl

--main = exitError "DIE !"

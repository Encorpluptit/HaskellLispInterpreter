module REPL where

import Environment
import System.Console.Haskeline
--import System.Console.Repline
--import System.Console.Repl
import Data.Foldable
import Parser
import Errors

--repl :: Env -> InputT IO ()
--repl env = do
--    maybeLine <- getInputLine "|λ〉"
--    forM_ maybeLine outputStrLn

--    case maybeLine of
--        Nothing   -> return () -- EOF / ctrl+d
--        Just line -> outputStrLn line
--            exprsOrErr <- return $ do
--                tokens <- tokenize line
--                parseExprs tokens

--type Repl a = HaskelineT IO a
--
---- Commands
--help :: [String] -> IO ()
--help args = print $ "Help: " ++ show args
--
--say :: [String] -> IO ()
--say args = do
--  _ <- system $ "cowsay" ++ " " ++ unwords args
--  return ()
--
--options :: [(String, [String] -> IO ())]
--options = [
--    ("help", help)  -- :help
--  , ("say", say)    -- :say
--  ]


--repl :: InputT IO ()
--repl = do
--    maybeLine <- getInputLine "|λ〉"
--    maybeLine <- getInputLine "|>"
--    forM_ maybeLine outputStrLn
--    forM_ maybeLine outputStrLn
--    case maybeLine of
--        Nothing   -> outputStrLn "Bye."-- EOF / ctrl+d
--        Just line -> process line >> repl
--        Just line -> outputStrLn line

--process :: String -> IO ()
--process str = do
--  res <- parseExpr str
--  either putStrLn return res

--replAlt :: IO ()
--replAlt = evalReplOpts $ ReplOpts
--  { banner           = const $ pure ">>> "
--  , command          = cmd
--  , options          = opts
--  , prefix           = Just ':'
--  , multilineCommand = Just "paste"
--  , tabComplete      = (Word0 completer)
--  , initialiser      = ini
--  , finaliser        = final
--  }


process :: String -> String
process s = case unpackError $ parseExpr s of
        Right x -> show x
        Left err -> show err

launchRepl :: IO ()
--launchRepl = runInputT defaultSettings loop
launchRepl = runInputT halSettings loop
    where
        loop :: InputT IO ()
        loop = do
            minput <- getInputLine "|λ〉"
            case minput of
                Nothing -> return ()
                Just "quit" -> outputStrLn "Bye."
                Just input -> do outputStrLn (process input) >> loop
        halSettings = Settings {
                complete = completeFilename,
                historyFile = Nothing,
                autoAddHistory = True
            }

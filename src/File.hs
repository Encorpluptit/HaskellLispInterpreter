module File where

import Control.Exception
import PrintUtils

loadFile :: String -> IO String
loadFile fp = do
    -- TODO: Use just "try" without handler ?
    except <- tryJust handler (readFile fp)
    case except of
        Left err -> writeErrorAndExit err
        Right content -> return content
    where
        handler :: IOError -> Maybe String
        handler err = Just "can't read file."

getArgsFiles :: [String] -> IO [String]
getArgsFiles = mapM loadFile
--getArgsFiles = foldr ((:) . (>>=)) loadFile
--getArgsFiles [] = return []
--getArgsFiles (x:xs) = do
--    file <- loadFile x
--    rest <- getArgsFiles xs
--    return (file: rest)

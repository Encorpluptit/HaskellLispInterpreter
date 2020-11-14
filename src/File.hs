module File where

import Control.Exception
import PrintUtils
import GHC.IO.Exception (ioe_description, ioe_filename, ioe_location)

loadFile :: String -> IO String
loadFile fp = do
    -- TODO: Use just "try" without handler ?
    except <- tryJust handler (readFile fp)
    case except of
        Left err -> writeErrorAndExit err
        Right content -> return content
    where
        handler :: IOError -> Maybe String
        handler err = Just $ show err

getArgsFiles :: [String] -> IO [String]
getArgsFiles = mapM loadFile

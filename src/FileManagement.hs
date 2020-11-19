module FileManagement where

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
    handler err = Just $ show err
--loadFile :: String -> ThrowsHalExprError String
--loadFile fp =
--    liftIO $ try $ readFile fp
--  -- TODO: Use just "try" without handler ?
--  either (HalError.throw . FileError) return (liftIO $ try $ readFile fp)
--  except <- try $ readFile fp
--  case except of
--    Left err -> HalError.throw $ FileError err
--    Right content -> return content
--  where
--    handler :: IOError -> Maybe String
--    handler err = Just $ show err

--getArgsFiles :: [String] -> IO [String]
--getArgsFiles = mapM loadFile

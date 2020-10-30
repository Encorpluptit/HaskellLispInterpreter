module Main where

import LibParsing
import ExitError

--main :: IO ()
--main = do
--    someFunc
--    case runParser parseInt "-42a" of
--        Left a  -> putStrLn a
--        Right b -> print b

main :: IO ()
main = exitError "DIE !"

module Errors
( HALError(..)
, ThrowsError
, throw
) where

import DataTypes
--import Control.Exception

-- | -----------------------------------------------------------------------------------------------------------------
-- ressources
-- https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/10_Error_Handling


data HALError = UnknownError    String          -- generic Error
--                | NumArgs       String Int      -- (eq? 1)
                | NbArgsError   String Integer LispVal     -- (eq? 1)
                | TypeError     String  LispVal          -- (eq? 1 "l")
                | UnboundVar    String          -- (eq? foo 1)   { foo not defined }
                | BuiltinError  String [LispVal]
--                | NotFunction   String          -- (foo 1)       { (define foo 0) }
                | KeywordError  LispVal  -- Useful with unbound Var ?
                | SyntaxError   String          -- (define foo 0
--                | FileError     String          -- file don't exist ? Wrong Syntax in file ?

instance Show HALError where
    show = showHALError

showHALError :: HALError -> String
showHALError (UnknownError msg) = "Unknown Error: " ++ msg
showHALError (TypeError msg val) = "Wrong type: " ++ msg ++ " -> " ++ show val
showHALError (NbArgsError op nb val) = "Wrong Number of Args in operator { " ++ op ++ " }. Expected "
    ++ show nb ++ " operand(s) -> Got: " ++ show val
showHALError (UnboundVar var) = "UnboundVar: " ++ var
showHALError (BuiltinError builtin args) = "Unrecognised " ++ builtin ++ " (built-in) args: " ++ show args
showHALError (SyntaxError msg) = msg
showHALError (KeywordError val) = "KeyWord Error, got : " ++ show val

newtype ThrowsError a = HandleError (Either HALError a)
    deriving(Show)

instance Functor ThrowsError where
    fmap _ (HandleError (Left err))     = HandleError $ Left err
    fmap f (HandleError (Right val))    = HandleError . Right $ f val

instance Applicative ThrowsError where
    pure = HandleError . Right
    (HandleError (Left err)) <*> _  = HandleError (Left err)
    (HandleError (Right f)) <*> x   = fmap f x

instance Monad ThrowsError where
    (HandleError te) >>= f =
        case te of
            Left err -> HandleError (Left err)
            Right val -> f val

    return val = HandleError $ Right val
--    fail err = TE . Left $ Error err

throw :: HALError -> ThrowsError a
throw err = HandleError $ Left err



--data LispException
--  = NumArgs Integer [LispVal]
--  | LengthOfList T.Text Int
--  | ExpectedList T.Text
--  | TypeMismatch T.Text LispVal
--  | BadSpecialForm T.Text
--  | NotFunction LispVal
--  | UnboundVar T.Text
--  | Default LispVal
--  | PError String -- from show anyway
--  | IOError T.Text
--
--showError :: LispException -> T.Text
--showError err =
--  case err of
--    (IOError txt)            -> T.concat ["Error reading file: ", txt]
--    (NumArgs int args)       -> T.concat ["Error Number Arguments, expected ", T.pack $ show int, " recieved args: ", unwordsList args]
--    (LengthOfList txt int)   -> T.concat ["Error Length of List in ", txt, " length: ", T.pack $ show int]
--    (ExpectedList txt)       -> T.concat ["Error Expected List in funciton ", txt]
--    (TypeMismatch txt val)   -> T.concat ["Error Type Mismatch: ", txt, showVal val]
--    (BadSpecialForm txt)     -> T.concat ["Error Bad Special Form: ", txt]
--    (NotFunction val)        -> T.concat ["Error Not a Function: ", showVal val]
--    (UnboundVar txt)         -> T.concat ["Error Unbound Variable: ", txt]
--    (PError str)             -> T.concat ["Parser Error, expression cannot evaluate: ",T.pack str]
--    (Default val)            -> T.concat ["Error, Danger Will Robinson! Evaluation could not proceed!  ", showVal val]


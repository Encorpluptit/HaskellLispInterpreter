module Errors
  ( HALError (..),
    ThrowsError,
    unpackError,
    throw,
  )
where

import DataTypes

--import Control.Exception

-- | -----------------------------------------------------------------------------------------------------------------
-- resources:
-- https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/10_Error_Handling

data HALError
  = UnknownError String -- generic Error
  | ParsingError LispVal String -- (eq? 1)
  | NbArgsError String Integer [LispVal] -- (eq? 1)
  | TypeError String LispVal -- (eq? 1 "l")
  | UnboundVar String -- (eq? foo 1)   { foo not defined }
  | BuiltinError String [LispVal]
  -- | NotFunction   String          -- (foo 1)       { (define foo 0) }
  | KeywordError LispVal -- Useful with unbound Var ?
  | SyntaxError String -- (define foo 0
  -- | FileError     String          -- file don't exist ? Wrong Syntax in file ?

instance Show HALError where
  show = showHALError

showHALError :: HALError -> String
showHALError (UnknownError msg) = "Unknown Error: " ++ msg
showHALError (ParsingError parsed left) = "Parsing Error: Parsed:\n" ++ show parsed ++ "\nLeft: "++ show left
showHALError (TypeError msg val) = "Wrong type: " ++ msg ++ " -> " ++ show val
showHALError (NbArgsError op nb val) =
  "Wrong Number of Args in operator { " ++ op ++ " }. Expected "
    ++ show nb
    ++ " operand(s) -> Got: " ++ show (length val) ++ " -> "
    ++ show val
showHALError (UnboundVar var) = "UnboundVar: " ++ var
showHALError (BuiltinError builtin args) = "Unrecognised " ++ builtin ++ " (built-in) args: " ++ show args
showHALError (SyntaxError msg) = msg
showHALError (KeywordError val) = "KeyWord Error, got : " ++ show val

newtype ThrowsError a = HandleError (Either HALError a)
  deriving (Show)

instance Functor ThrowsError where
  fmap _ (HandleError (Left err)) = HandleError $ Left err
  fmap f (HandleError (Right val)) = HandleError . Right $ f val

instance Applicative ThrowsError where
  pure = HandleError . Right
  (HandleError (Left err)) <*> _ = HandleError (Left err)
  (HandleError (Right f)) <*> x = fmap f x

instance Monad ThrowsError where
  (HandleError te) >>= f =
    case te of
      Left err -> HandleError (Left err)
      Right val -> f val

  return val = HandleError $ Right val

throw :: HALError -> ThrowsError a
throw err = HandleError $ Left err

unpackError :: ThrowsError a -> Either HALError a
unpackError (HandleError val) = val

module HalError where

import Control.Applicative

data HALError a
  = UnknownError String
  | ParsingError a String
  | NbArgsError String Integer [a]
  | TypeError String a
  | UnboundVar a
  | BuiltinError String [a]
  | NotFunction a
  | KeywordError a
  | SyntaxError String
  | FileError String
  deriving (Eq)

--    deriving (Eq)

instance (Show a) => Show (HALError a) where
  show = showHALError

--showHALError :: HALError a -> String
showHALError :: Show a => HALError a -> String
showHALError (UnknownError msg) = "Unknown Error: " ++ msg
showHALError (ParsingError parsed left) = "Parsing Error: Parsed:\n" ++ show parsed ++ "\nLeft: " ++ show left
showHALError (TypeError msg val) = "Wrong type: " ++ msg ++ " -> " ++ show val
showHALError (NbArgsError op nb val) =
  "Wrong Number of Args in operator { " ++ op ++ " }. Expected "
    ++ show nb
    ++ " operand(s) -> Got: "
    ++ show (length val)
    ++ " -> "
    ++ show val
showHALError (UnboundVar var) = show var ++ " : not defined"
showHALError (BuiltinError builtin args) = "Unrecognised " ++ builtin ++ " (built-in) args: " ++ show args
showHALError (SyntaxError msg) = msg
showHALError (KeywordError val) = "KeyWord Error, got : " ++ show val
showHALError (NotFunction val) = "attempt to apply non-procedure #t" ++ " : " ++ show val
showHALError (FileError val) = "Problem reading content of " ++ " : " ++ show val


newtype ThrowsError a b = HandleError (Either (HALError a) b)
  --  deriving (Show, Eq)
  deriving (Show)

instance Functor (ThrowsError a) where
  fmap _ (HandleError (Left err)) = HandleError $ Left err
  fmap f (HandleError (Right val)) = HandleError . Right $ f val

instance Applicative (ThrowsError a) where
  pure = HandleError . Right
  (HandleError (Left err)) <*> _ = HandleError (Left err)
  (HandleError (Right f)) <*> x = fmap f x

instance Alternative (ThrowsError a) where
  empty = HandleError . Left $ UnknownError "parser Empty"
  (HandleError a) <|> (HandleError b) = case a of
    Right val -> HandleError (Right val)
    Left err -> case b of
      Right val -> HandleError (Right val)
      Left _ -> HandleError (Left err)

instance Monad (ThrowsError a) where
  (HandleError te) >>= f =
    case te of
      Left err -> HandleError (Left err)
      Right val -> f val
  return val = HandleError $ Right val

throw :: HALError a -> ThrowsError a b
throw err = HandleError $ Left err

unpackError :: ThrowsError a b -> Either (HALError a) b
unpackError (HandleError val) = val

--liftError :: ThrowsError a b -> c -> ThrowsError c b
--liftError (HandleError val) = liftA

--class (Monad m) => (ThrowsError m) where
--    liftError :: IO a -> m a

module Builtins
(
    getBuiltins
    , builtins
  , unpackBoolean
)
where

import DataTypes

-- | TODO: ??

-- | -----------------------------------------------------------------------------------------------------------------
-- Scheme Reference:
--  - https://schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html

type BinaryOperator a = (a -> a -> a)

--type BoolBinaryOperator a = (a -> a -> Bool)

getBuiltins :: Env -> Identifier -> ThrowsError (LispVal, Env)
getBuiltins env ident =  case lookup ident builtins of
    Just a -> return (Func env (LispFct fct), env)
        where fct _ = a
    Nothing -> throw $ UnboundVar ident

builtins :: [(String, [LispVal] -> ThrowsError LispVal)]
--builtins :: Env
builtins = [
    ("+", numericBinaryOp "+" (+)),
    ("-", numericBinaryOp "-" (-)),
    ("*", numericBinaryOp "*" (*)),
--    TODO: != func for mod and div (take only 2 args but here can manage with many
    ("div", numericBinaryOp "div" div),
    ("mod", numericBinaryOp "mod" mod),
    ("<", numericBoolExpr "<" (<)),
    ("atom?", unaryOp "atom?" isAtom),
    ("car", car),
    ("cdr", cdr),
    ("cons", cons),
    ("eq?", equal),
-- | -----------------------------------------------------------------------------------------------------------------
-- Bonuses:
    ("=", numericBoolExpr "=" (==)),
    (">", numericBoolExpr ">" (>)),
    (">=", numericBoolExpr ">=" (>=)),
    ("<=", numericBoolExpr "<=" (<=)),
    ("!=", numericBoolExpr "!=" (/=)),
    ("remainder", numericBinaryOp "remainder" rem),
    ("quotient", numericBinaryOp "quotient" quot),
    ("number?", unaryOp "quotient" isNumber),
    ("bool?", unaryOp "bool?" isBool),
    ("list?", unaryOp "list?" isList),
    ("string?", unaryOp "string?" isString),
    ("symbol?", unaryOp "symbol?" isAtom),
    ("||", boolBoolExpr "||" (||)),
    ("&&", boolBoolExpr "&&" (&&)),
    ("string=?", stringBoolExpr "string=?" (==)),
    ("string>?", stringBoolExpr "string=?" (>)),
    ("string<?", stringBoolExpr "string=?" (<)),
    ("string<=?", stringBoolExpr "string=?" (<=)),
    ("string>=?", stringBoolExpr "string=?" (>=)),
    ("string-length", stringLength)
    ]
-- | -----------------------------------------------------------------------------------------------------------------
-- TODO: Add the following built-ins:
--  * More list built-ins (pair, ...):
--      - https://schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.3.2
--  * More Boolean built-ins (not, ...) :
--      - https://schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.3.1
-- TODO: Manage:
--  * Different types (Ex: Double + Integer)
-- TODO: Add following not standard primitives
--  * foldl (Haskell reference)
--  * foldl1 (On more function and lambdas, already managed for op like (+), (-))



-- | -----------------------------------------------------------------------------------------------------------------
-- Operators:
-- * foldl1:
--  http://zvon.org/other/haskell/Outputprelude/foldl1_f.html
-- * mapM:
--  http://zvon.org/other/haskell/Outputprelude/mapM_f.html
numericBinaryOp :: String -> BinaryOperator Integer -> [LispVal] -> ThrowsError LispVal
numericBinaryOp op _ [] = throw $ NbArgsError op 2 []
--numericBinaryOp "+" fct [val] = ValNum (fct <$> 0 <*> unpackNumeric "+" val)
--numericBinaryOp "+" fct [val] = ValNum . foldl1 fct <$> mapM (unpackNumeric "+") (ValNum 0: [val])
-- TODO: Remove this HotFIX
numericBinaryOp "+" fct [ValNum val] = return $ ValNum (0 `fct` val)
numericBinaryOp "-" fct [ValNum val] = return $ ValNum (0 `fct` val)
numericBinaryOp "*" fct [ValNum val] = return $ ValNum (1 `fct` val)
numericBinaryOp "div" fct [ValNum val] = return $ ValNum (1 `fct` val)
-- END: Remove this HotFIX
numericBinaryOp op _ [val] = throw $ NbArgsError op 2 [val]
-- | TODO: manage op between != types (replace BinaryOperator Integer -> BinaryOperator LispNum ?)
-- | Equivalents:
--numericBinOp op params = mapM unpackNum params >>= return . ValNum . foldl1 op
-- TODO: Remove this HotFIX
numericBinaryOp "div" fct params = ValNum . foldl1 hotFix <$> mapM (unpackNumeric "div") params
    where hotFix a b
            | b < 0 = negate $ fct a (negate b)
            | otherwise = fct a b
-- END: Remove this HotFIX
-- TODO: Modify this for hotfix
numericBinaryOp op fct params = ValNum . foldl1 fct <$> mapM (unpackNumeric op) params

boolBinaryOp :: String -> (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinaryOp _ unpacker op [a, b] = do
    left <- unpacker a
    right <- unpacker b
    return $ ValBool $ left `op` right
boolBinaryOp procedure _ _ args = throw $ NbArgsError procedure 2 args

unaryOp :: String -> (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp _ fct [val] = return (fct val)
unaryOp op _ args   = throw $ NbArgsError op 1 args


-- | -----------------------------------------------------------------------------------------------------------------
-- Unary Operator type testers
-- No Need for error handling here
isNumber :: LispVal -> LispVal
isNumber (ValNum _)    = ValBool True
isNumber _             = ValBool False

isBool :: LispVal -> LispVal
isBool (ValBool _)  = ValBool True
isBool _            = ValBool False

isList :: LispVal -> LispVal
isList (ValList _)  = ValBool True
isList _            = ValBool False

isString :: LispVal -> LispVal
isString (ValString _)  = ValBool True
isString _              = ValBool False

isAtom :: LispVal -> LispVal
isAtom (Atom _) = ValBool True
isAtom (ValList []) = ValBool True
isAtom _        = ValBool False


-- | -----------------------------------------------------------------------------------------------------------------
-- Binary Operator helper(s)
unpackNumeric :: String -> LispVal -> ThrowsError Integer
unpackNumeric _ (ValNum nb) = return nb
unpackNumeric s (ValList [n]) = unpackNumeric s n
-- TODO: improve error reporting or manage several types ? (Improve TypeError or remove "procedure" arg here and in Boolean Expr helpers).
unpackNumeric procedure value   = throw $ TypeError "Mismatch Value when unpacking Integer" value

unpackString :: String -> LispVal -> ThrowsError String
unpackString _ (ValString str)  = return str
-- TODO: Interpret Booleans and Numbers as String ?
--unpackString _ (ValBool str)    = return $ show str
--unpackString _ (ValNum str)     = return $ show str
unpackString procedure value    = throw $ TypeError "Mismatch Value when unpacking String" value

unpackBoolean :: String -> LispVal -> ThrowsError Bool
unpackBoolean _ (ValBool bool)  = return bool
unpackBoolean procedure value   = throw $ TypeError "Mismatch Value when unpacking Bool" value


-- | -----------------------------------------------------------------------------------------------------------------
-- Boolean Expression Binary Operator helpers
numericBoolExpr :: String -> (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numericBoolExpr procedure = boolBinaryOp procedure (unpackNumeric procedure)

boolBoolExpr :: String -> (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolExpr procedure = boolBinaryOp procedure (unpackBoolean procedure)

stringBoolExpr :: String -> (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
stringBoolExpr procedure = boolBinaryOp procedure (unpackString procedure)


-- | -----------------------------------------------------------------------------------------------------------------
-- Takes a cons as argument, returns its first element (the car).
-- TODO: Improve Doc (with Example ??)
car :: [LispVal] -> ThrowsError LispVal
car [ValList (x:_)] = return x
car [ValDottedList (x:_) _] = return x
car [val] = throw $ TypeError "Incorrect Value Type when applying car" val
car args = throw $ NbArgsError "car" 1 args

-- | -----------------------------------------------------------------------------------------------------------------
-- Takes a cons as argument, returns its second element (the cdr).
-- TODO: Improve Doc (with Example ??)
cdr :: [LispVal] -> ThrowsError LispVal
cdr [ValList (_:xs)] = return $ ValList xs
cdr [ValDottedList [_] x] = return x
cdr [ValDottedList (_:xs) x] = return $ ValDottedList xs x
cdr [val] = throw $ TypeError "Incorrect Value Type when applying cdr" val
cdr args = throw $ NbArgsError "cdr" 1 args

-- | -----------------------------------------------------------------------------------------------------------------
-- Takes two arguments, construct a new list cell with the first argument in the first place (car) and the second
-- argument is the second place (cdr).
-- TODO: Improve Doc (with Example ??)
cons :: [LispVal] -> ThrowsError LispVal
cons [x, ValList []] = return $ ValList [x]
cons [x, ValList xs] = return $ ValList (x:xs)
cons [x, ValDottedList xs end] = return $ ValDottedList (x:xs) end
cons [a, b] = return $ ValDottedList [a] b
cons args = throw $ NbArgsError "cons" 2 args


-- | -----------------------------------------------------------------------------------------------------------------
-- Equals
-- TODO: [BONUS] Manage "equal?" built-in ? Weak type checking
equal :: [LispVal] -> ThrowsError LispVal
equal [Atom a, Atom b]              = return $ ValBool $ a == b
equal [ValNum a, ValNum b]          = return $ ValBool $ a == b
equal [ValBool a, ValBool b]        = return $ ValBool $ a == b
equal [ValString a, ValString b]    = return $ ValBool $ a == b
equal [ValList [], ValList []]      = return $ ValBool True
-- TODO: [BONUS] Manage Equal List ? Use zip to create a list of bool and apply "all" on it ?
--equal [ValList a, ValList b]        = return $ ValBool res
--    where
--        res = (length a == length b) && True -- put fct here and define in where
equal [_, _]                        = return $ ValBool False
equal value                         = throw $ NbArgsError "eq?" 2 value


-- | -----------------------------------------------------------------------------------------------------------------
-- Strings built-ins
stringLength :: [LispVal] -> ThrowsError LispVal
stringLength [ValString str] = return $ ValNum $ fromIntegral $ length str
stringLength [value] = throw $ TypeError "Mismatch Value when unpacking Bool" value
stringLength args = throw $ NbArgsError "string-length" 1 args
-- TODO: [BONUS] Add stringRef fct

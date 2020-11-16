module EvalTestsSpec where

import DataTypes
import Parser
import Test.Hspec

spec :: Spec
spec = describe "Eval Testing" $ do
  testEquals
  testIfs
  testArithmetic

expectRightValue :: LispVal -> Either HALError (LispVal, Env) -> Bool
expectRightValue expected res = case res of
  Left _ -> False
  Right (result, _) -> result == expected

chainAssertion :: Env -> [(String, LispVal)] -> Expectation
chainAssertion _ [] = return ()
chainAssertion env ((expr, expected) : xs) = do
  case unpackError (parseExpr env expr) of
    Left err -> expectationFailure $ "Failure in chainAssertion, got { " ++ show err ++ "}, Expected {" ++ show expected ++ " }"
    Right (result, newEnv) -> (result `shouldBe` expected) >> chainAssertion newEnv xs

testEquals :: Spec
testEquals =
  describe "Tests for equality between values" $ do
    it "(eq? 2 2) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [("(eq? 2 2)", ValBool True)]

    it "(define foo 2) && (eq? foo 2) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 2)", Atom "foo"),
          ("(eq? foo 2)", ValBool True)
        ]

    it "(define foo 2) && (eq? 2 foo) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 2)", Atom "foo"),
          ("(eq? 2 foo)", ValBool True)
        ]

    it "(define foo 2) && (define bar 2) && (eq? foo bar) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 2)", Atom "foo"),
          ("(define bar 2)", Atom "bar"),
          ("(eq? foo bar)", ValBool True)
        ]

    it "(define foo 2) && (define bar 2) && (eq? bar foo) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 2)", Atom "foo"),
          ("(define bar 2)", Atom "bar"),
          ("(eq? bar foo)", ValBool True)
        ]

    it "(eq? 2 3) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [("(eq? 2 3)", ValBool False)]

    it "(define foo 2) && (eq? foo 3) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 2)", Atom "foo"),
          ("(eq? foo 3)", ValBool False)
        ]

    it "(define foo 3) && (eq? 2 foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 3)", Atom "foo"),
          ("(eq? 2 foo)", ValBool False)
        ]

    it "(define foo 2) && (define bar 3) && (eq? foo bar) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 2)", Atom "foo"),
          ("(define bar 3)", Atom "bar"),
          ("(eq? foo bar)", ValBool False)
        ]

    it "(define foo 3) && (define bar 2) && (eq? foo bar) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 3)", Atom "foo"),
          ("(define bar 2)", Atom "bar"),
          ("(eq? foo bar)", ValBool False)
        ]

    it "(define foo 2) && (define bar 3) && (eq? bar foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 2)", Atom "foo"),
          ("(define bar 3)", Atom "bar"),
          ("(eq? bar foo)", ValBool False)
        ]

    it "(define foo 3) && (define bar 2) && (eq? bar foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 3)", Atom "foo"),
          ("(define bar 2)", Atom "bar"),
          ("(eq? bar foo)", ValBool False)
        ]

    it "(define foo 'a) && (define foo 3) && (define bar 2) && (eq? bar foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 'a)", Atom "foo"),
          ("(define foo 3)", Atom "foo"),
          ("(define bar 2)", Atom "bar"),
          ("(eq? bar foo)", ValBool False)
        ]

    it "(define bar 'a) && (define foo 3) && (define bar 2) && (eq? bar foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define bar 'a)", Atom "bar"),
          ("(define foo 3)", Atom "foo"),
          ("(define bar 2)", Atom "bar"),
          ("(eq? bar foo)", ValBool False)
        ]

    it "(define foo 'a) && (define bar 'a) && (define foo 3) && (define bar 2) && (eq? bar foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 'a)", Atom "foo"),
          ("(define bar 'a)", Atom "bar"),
          ("(define foo 3)", Atom "foo"),
          ("(define bar 2)", Atom "bar"),
          ("(eq? bar foo)", ValBool False)
        ]

    it "(eq? 200000000000000 200000000000000) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [("(eq? 200000000000000 200000000000000)", ValBool True)]

    it "(define foo 200000000000000) && (eq? foo 200000000000000) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 200000000000000)", Atom "foo"),
          ("(eq? foo 200000000000000)", ValBool True)
        ]

    it "(define foo 200000000000000) && (eq? 200000000000000 foo) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 200000000000000)", Atom "foo"),
          ("(eq? 200000000000000 foo)", ValBool True)
        ]

    it "(define foo 200000000000000) && (define bar 200000000000000) && (eq? foo bar) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 200000000000000)", Atom "foo"),
          ("(define bar 200000000000000)", Atom "bar"),
          ("(eq? foo bar)", ValBool True)
        ]

    it "(define foo 200000000000000) && (define bar 200000000000000) && (eq? bar foo) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 200000000000000)", Atom "foo"),
          ("(define bar 200000000000000)", Atom "bar"),
          ("(eq? bar foo)", ValBool True)
        ]

    it "(eq? 200000000000000 200000000000001) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [("(eq? 200000000000000 200000000000001)", ValBool False)]

    it "(define foo 200000000000000) && (eq? foo 200000000000001) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 200000000000000)", Atom "foo"),
          ("(eq? foo 200000000000001)", ValBool False)
        ]

    it "(define foo 200000000000001) && (eq? 200000000000000 foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 200000000000001)", Atom "foo"),
          ("(eq? 200000000000000 foo)", ValBool False)
        ]

    it "(define foo 200000000000000) && (define bar 200000000000001) && (eq? foo bar) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 200000000000000)", Atom "foo"),
          ("(define bar 200000000000001)", Atom "bar"),
          ("(eq? foo bar)", ValBool False)
        ]

    it "(define foo 200000000000001) && (define bar 200000000000000) && (eq? foo bar) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 200000000000001)", Atom "foo"),
          ("(define bar 200000000000000)", Atom "bar"),
          ("(eq? foo bar)", ValBool False)
        ]

    it "(define foo 200000000000000) && (define bar 200000000000001) && (eq? bar foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 200000000000000)", Atom "foo"),
          ("(define bar 200000000000001)", Atom "bar"),
          ("(eq? bar foo)", ValBool False)
        ]

    it "(define foo 200000000000001) && (define bar 200000000000000) && (eq? bar foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 200000000000001)", Atom "foo"),
          ("(define bar 200000000000000)", Atom "bar"),
          ("(eq? bar foo)", ValBool False)
        ]

    it "(define foo 'a) && (define foo 200000000000000001) && (define bar 200000000000000000) && (eq? bar foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 'a)", Atom "foo"),
          ("(define foo 200000000000000001)", Atom "foo"),
          ("(define bar 200000000000000000)", Atom "bar"),
          ("(eq? bar foo)", ValBool False)
        ]

    it "(define bar 'a) && (define foo 200000000000000001) && (define bar 200000000000000000) && (eq? bar foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define bar 'a)", Atom "bar"),
          ("(define foo 200000000000000001)", Atom "foo"),
          ("(define bar 200000000000000000)", Atom "bar"),
          ("(eq? bar foo)", ValBool False)
        ]

    it "(define foo 'a) && (define bar 'a) && (define foo 200000000000000001) && (define bar 200000000000000000) && (eq? bar foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 'a)", Atom "foo"),
          ("(define bar 'a)", Atom "bar"),
          ("(define foo 200000000000000001)", Atom "foo"),
          ("(define bar 200000000000000000)", Atom "bar"),
          ("(eq? bar foo)", ValBool False)
        ]

    it "(eq? 20000 20000) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [("(eq? 20000 20000)", ValBool True)]

    it "(define foo 20000) && (eq? foo 20000) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 20000)", Atom "foo"),
          ("(eq? foo 20000)", ValBool True)
        ]

    it "(define foo 20000) && (eq? 20000 foo) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 20000)", Atom "foo"),
          ("(eq? 20000 foo)", ValBool True)
        ]

    it "(define foo 20000) && (define bar 20000) && (eq? foo bar) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 20000)", Atom "foo"),
          ("(define bar 20000)", Atom "bar"),
          ("(eq? foo bar)", ValBool True)
        ]

    it "(define foo 20000) && (define bar 20000) && (eq? bar foo) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 20000)", Atom "foo"),
          ("(define bar 20000)", Atom "bar"),
          ("(eq? bar foo)", ValBool True)
        ]

    it "(eq? 20000 -100) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [("(eq? 20000 -100)", ValBool False)]

    it "(define foo 20000) && (eq? foo -100) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 20000)", Atom "foo"),
          ("(eq? foo -100)", ValBool False)
        ]

    it "(define foo -100) && (eq? 20000 foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo -100)", Atom "foo"),
          ("(eq? 20000 foo)", ValBool False)
        ]

    it "(define foo 20000) && (define bar -100) && (eq? foo bar) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 20000)", Atom "foo"),
          ("(define bar -100)", Atom "bar"),
          ("(eq? foo bar)", ValBool False)
        ]

    it "(define foo -100) && (define bar 20000) && (eq? foo bar) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo -100)", Atom "foo"),
          ("(define bar 20000)", Atom "bar"),
          ("(eq? foo bar)", ValBool False)
        ]

    it "(define foo 20000) && (define bar -100) && (eq? bar foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 20000)", Atom "foo"),
          ("(define bar -100)", Atom "bar"),
          ("(eq? bar foo)", ValBool False)
        ]

    it "(define foo -100) && (define bar 20000) && (eq? bar foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo -100)", Atom "foo"),
          ("(define bar 20000)", Atom "bar"),
          ("(eq? bar foo)", ValBool False)
        ]

    it "(define foo 'a) && (define foo 20000) && (define bar -100) && (eq? bar foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 'a)", Atom "foo"),
          ("(define foo 20000)", Atom "foo"),
          ("(define bar -100)", Atom "bar"),
          ("(eq? bar foo)", ValBool False)
        ]

    it "(define bar 'a) && (define foo 20000) && (define bar -100) && (eq? bar foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define bar 'a)", Atom "bar"),
          ("(define foo 20000)", Atom "foo"),
          ("(define bar -100)", Atom "bar"),
          ("(eq? bar foo)", ValBool False)
        ]

    it "(define foo 'a) && (define bar 'a) && (define foo 20000) && (define bar -100) && (eq? bar foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 'a)", Atom "foo"),
          ("(define bar 'a)", Atom "bar"),
          ("(define foo 20000)", Atom "foo"),
          ("(define bar -100)", Atom "bar"),
          ("(eq? bar foo)", ValBool False)
        ]

    it "(eq? -1 -1) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [("(eq? -1 -1)", ValBool True)]

    it "(define foo -1) && (eq? foo -1) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo -1)", Atom "foo"),
          ("(eq? foo -1)", ValBool True)
        ]

    it "(define foo -1) && (eq? -1 foo) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo -1)", Atom "foo"),
          ("(eq? -1 foo)", ValBool True)
        ]

    it "(define foo -1) && (define bar -1) && (eq? foo bar) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo -1)", Atom "foo"),
          ("(define bar -1)", Atom "bar"),
          ("(eq? foo bar)", ValBool True)
        ]

    it "(define foo -1) && (define bar -1) && (eq? bar foo) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo -1)", Atom "foo"),
          ("(define bar -1)", Atom "bar"),
          ("(eq? bar foo)", ValBool True)
        ]

    it "(eq? -1 200000) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [("(eq? -1 200000)", ValBool False)]

    it "(define foo -1) && (eq? foo 200000) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo -1)", Atom "foo"),
          ("(eq? foo 200000)", ValBool False)
        ]

    it "(define foo 200000) && (eq? -1 foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 200000)", Atom "foo"),
          ("(eq? -1 foo)", ValBool False)
        ]

    it "(define foo -1) && (define bar 200000) && (eq? foo bar) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo -1)", Atom "foo"),
          ("(define bar 200000)", Atom "bar"),
          ("(eq? foo bar)", ValBool False)
        ]

    it "(define foo 200000) && (define bar -1) && (eq? foo bar) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 200000)", Atom "foo"),
          ("(define bar -1)", Atom "bar"),
          ("(eq? foo bar)", ValBool False)
        ]

    it "(define foo -1) && (define bar 200000) && (eq? bar foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo -1)", Atom "foo"),
          ("(define bar 200000)", Atom "bar"),
          ("(eq? bar foo)", ValBool False)
        ]

    it "(define foo 200000) && (define bar -1) && (eq? bar foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 200000)", Atom "foo"),
          ("(define bar -1)", Atom "bar"),
          ("(eq? bar foo)", ValBool False)
        ]

    it "(define foo 'a) && (define foo -1) && (define bar 200000) && (eq? bar foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 'a)", Atom "foo"),
          ("(define foo -1)", Atom "foo"),
          ("(define bar 200000)", Atom "bar"),
          ("(eq? bar foo)", ValBool False)
        ]

    it "(define bar 'a) && (define foo -1) && (define bar 200000) && (eq? bar foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define bar 'a)", Atom "bar"),
          ("(define foo -1)", Atom "foo"),
          ("(define bar 200000)", Atom "bar"),
          ("(eq? bar foo)", ValBool False)
        ]

    it "(define foo 'a) && (define bar 'a) && (define foo -1) && (define bar 200000) && (eq? bar foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 'a)", Atom "foo"),
          ("(define bar 'a)", Atom "bar"),
          ("(define foo -1)", Atom "foo"),
          ("(define bar 200000)", Atom "bar"),
          ("(eq? bar foo)", ValBool False)
        ]

    it "(eq? -2000 -2000) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [("(eq? -2000 -2000)", ValBool True)]

    it "(define foo -2000) && (eq? foo -2000) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo -2000)", Atom "foo"),
          ("(eq? foo -2000)", ValBool True)
        ]

    it "(define foo -2000) && (eq? -2000 foo) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo -2000)", Atom "foo"),
          ("(eq? -2000 foo)", ValBool True)
        ]

    it "(define foo -2000) && (define bar -2000) && (eq? foo bar) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo -2000)", Atom "foo"),
          ("(define bar -2000)", Atom "bar"),
          ("(eq? foo bar)", ValBool True)
        ]

    it "(define foo -2000) && (define bar -2000) && (eq? bar foo) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo -2000)", Atom "foo"),
          ("(define bar -2000)", Atom "bar"),
          ("(eq? bar foo)", ValBool True)
        ]

    it "(eq? -2000 -30000) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [("(eq? -2000 -30000)", ValBool False)]

    it "(define foo -2000) && (eq? foo -30000) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo -2000)", Atom "foo"),
          ("(eq? foo -30000)", ValBool False)
        ]

    it "(define foo -30000) && (eq? -2000 foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo -30000)", Atom "foo"),
          ("(eq? -2000 foo)", ValBool False)
        ]

    it "(define foo -2000) && (define bar -30000) && (eq? foo bar) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo -2000)", Atom "foo"),
          ("(define bar -30000)", Atom "bar"),
          ("(eq? foo bar)", ValBool False)
        ]

    it "(define foo -30000) && (define bar -2000) && (eq? foo bar) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo -30000)", Atom "foo"),
          ("(define bar -2000)", Atom "bar"),
          ("(eq? foo bar)", ValBool False)
        ]

    it "(define foo -2000) && (define bar -30000) && (eq? bar foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo -2000)", Atom "foo"),
          ("(define bar -30000)", Atom "bar"),
          ("(eq? bar foo)", ValBool False)
        ]

    it "(define foo -30000) && (define bar -2000) && (eq? bar foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo -30000)", Atom "foo"),
          ("(define bar -2000)", Atom "bar"),
          ("(eq? bar foo)", ValBool False)
        ]

    it "(define foo 'a) && (define foo -5000) && (define bar -30000) && (eq? bar foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 'a)", Atom "foo"),
          ("(define foo -5000)", Atom "foo"),
          ("(define bar -30000)", Atom "bar"),
          ("(eq? bar foo)", ValBool False)
        ]

    it "(define bar 'a) && (define foo -5000) && (define bar -30000) && (eq? bar foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define bar 'a)", Atom "bar"),
          ("(define foo -5000)", Atom "foo"),
          ("(define bar -30000)", Atom "bar"),
          ("(eq? bar foo)", ValBool False)
        ]

    it "(define foo 'a) && (define bar 'a) && (define foo -5000) && (define bar -30000) && (eq? bar foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 'a)", Atom "foo"),
          ("(define bar 'a)", Atom "bar"),
          ("(define foo -5000)", Atom "foo"),
          ("(define bar -30000)", Atom "bar"),
          ("(eq? bar foo)", ValBool False)
        ]

    it "(eq? 0 0) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [("(eq? 0 0)", ValBool True)]

    it "(define foo 0) && (eq? foo 0) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 0)", Atom "foo"),
          ("(eq? foo 0)", ValBool True)
        ]

    it "(define foo 0) && (eq? 0 foo) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 0)", Atom "foo"),
          ("(eq? 0 foo)", ValBool True)
        ]

    it "(define foo 0) && (define bar 0) && (eq? foo bar) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 0)", Atom "foo"),
          ("(define bar 0)", Atom "bar"),
          ("(eq? foo bar)", ValBool True)
        ]

    it "(define foo 0) && (define bar 0) && (eq? bar foo) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 0)", Atom "foo"),
          ("(define bar 0)", Atom "bar"),
          ("(eq? bar foo)", ValBool True)
        ]

    it "(eq? 0 3) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [("(eq? 0 3)", ValBool False)]

    it "(define foo 0) && (eq? foo 3) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 0)", Atom "foo"),
          ("(eq? foo 3)", ValBool False)
        ]

    it "(define foo 3) && (eq? 0 foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 3)", Atom "foo"),
          ("(eq? 0 foo)", ValBool False)
        ]

    it "(define foo 0) && (define bar 3) && (eq? foo bar) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 0)", Atom "foo"),
          ("(define bar 3)", Atom "bar"),
          ("(eq? foo bar)", ValBool False)
        ]

    it "(define foo 3) && (define bar 0) && (eq? foo bar) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 3)", Atom "foo"),
          ("(define bar 0)", Atom "bar"),
          ("(eq? foo bar)", ValBool False)
        ]

    it "(define foo 0) && (define bar 3) && (eq? bar foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 0)", Atom "foo"),
          ("(define bar 3)", Atom "bar"),
          ("(eq? bar foo)", ValBool False)
        ]

    it "(define foo 3) && (define bar 0) && (eq? bar foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 3)", Atom "foo"),
          ("(define bar 0)", Atom "bar"),
          ("(eq? bar foo)", ValBool False)
        ]

    it "(define foo 'a) && (define foo 3) && (define bar 0) && (eq? bar foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 'a)", Atom "foo"),
          ("(define foo 3)", Atom "foo"),
          ("(define bar 0)", Atom "bar"),
          ("(eq? bar foo)", ValBool False)
        ]

    it "(define bar 'a) && (define foo 3) && (define bar 0) && (eq? bar foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define bar 'a)", Atom "bar"),
          ("(define foo 3)", Atom "foo"),
          ("(define bar 0)", Atom "bar"),
          ("(eq? bar foo)", ValBool False)
        ]

    it "(define foo 'a) && (define bar 'a) && (define foo 3) && (define bar 0) && (eq? bar foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 'a)", Atom "foo"),
          ("(define bar 'a)", Atom "bar"),
          ("(define foo 3)", Atom "foo"),
          ("(define bar 0)", Atom "bar"),
          ("(eq? bar foo)", ValBool False)
        ]

    it "(eq? 'foo 'foo) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [("(eq? 'foo 'foo)", ValBool True)]

    it "(define foo 'foo) && (eq? foo 'foo) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 'foo)", Atom "foo"),
          ("(eq? foo 'foo)", ValBool True)
        ]

    it "(define foo 'foo) && (eq? 'foo foo) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 'foo)", Atom "foo"),
          ("(eq? 'foo foo)", ValBool True)
        ]

    it "(define foo 'foo) && (define bar 'foo) && (eq? foo bar) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 'foo)", Atom "foo"),
          ("(define bar 'foo)", Atom "bar"),
          ("(eq? foo bar)", ValBool True)
        ]

    it "(define foo 'foo) && (define bar 'foo) && (eq? bar foo) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 'foo)", Atom "foo"),
          ("(define bar 'foo)", Atom "bar"),
          ("(eq? bar foo)", ValBool True)
        ]

    it "(eq? 'foo 'bar) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [("(eq? 'foo 'bar)", ValBool False)]

    it "(define foo 'foo) && (eq? foo 'bar) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 'foo)", Atom "foo"),
          ("(eq? foo 'bar)", ValBool False)
        ]

    it "(define foo 'bar) && (eq? 'foo foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 'bar)", Atom "foo"),
          ("(eq? 'foo foo)", ValBool False)
        ]

    it "(define foo 'foo) && (define bar 'bar) && (eq? foo bar) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 'foo)", Atom "foo"),
          ("(define bar 'bar)", Atom "bar"),
          ("(eq? foo bar)", ValBool False)
        ]

    it "(define foo 'bar) && (define bar 'foo) && (eq? foo bar) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 'bar)", Atom "foo"),
          ("(define bar 'foo)", Atom "bar"),
          ("(eq? foo bar)", ValBool False)
        ]

    it "(define foo 'foo) && (define bar 'bar) && (eq? bar foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 'foo)", Atom "foo"),
          ("(define bar 'bar)", Atom "bar"),
          ("(eq? bar foo)", ValBool False)
        ]

    it "(define foo 'bar) && (define bar 'foo) && (eq? bar foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 'bar)", Atom "foo"),
          ("(define bar 'foo)", Atom "bar"),
          ("(eq? bar foo)", ValBool False)
        ]

    it "(define foo 'a) && (define foo 'bar) && (define bar 'foo) && (eq? bar foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 'a)", Atom "foo"),
          ("(define foo 'bar)", Atom "foo"),
          ("(define bar 'foo)", Atom "bar"),
          ("(eq? bar foo)", ValBool False)
        ]

    it "(define bar 'a) && (define foo 'bar) && (define bar 'foo) && (eq? bar foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define bar 'a)", Atom "bar"),
          ("(define foo 'bar)", Atom "foo"),
          ("(define bar 'foo)", Atom "bar"),
          ("(eq? bar foo)", ValBool False)
        ]

    it "(define foo 'a) && (define bar 'a) && (define foo 'bar) && (define bar 'foo) && (eq? bar foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 'a)", Atom "foo"),
          ("(define bar 'a)", Atom "bar"),
          ("(define foo 'bar)", Atom "foo"),
          ("(define bar 'foo)", Atom "bar"),
          ("(eq? bar foo)", ValBool False)
        ]

    it "(eq? 'lol 'lol) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [("(eq? 'lol 'lol)", ValBool True)]

    it "(define foo 'lol) && (eq? foo 'lol) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 'lol)", Atom "foo"),
          ("(eq? foo 'lol)", ValBool True)
        ]

    it "(define foo 'lol) && (eq? 'lol foo) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 'lol)", Atom "foo"),
          ("(eq? 'lol foo)", ValBool True)
        ]

    it "(define foo 'lol) && (define bar 'lol) && (eq? foo bar) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 'lol)", Atom "foo"),
          ("(define bar 'lol)", Atom "bar"),
          ("(eq? foo bar)", ValBool True)
        ]

    it "(define foo 'lol) && (define bar 'lol) && (eq? bar foo) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 'lol)", Atom "foo"),
          ("(define bar 'lol)", Atom "bar"),
          ("(eq? bar foo)", ValBool True)
        ]

    it "(eq? 'lol 'lola) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [("(eq? 'lol 'lola)", ValBool False)]

    it "(define foo 'lol) && (eq? foo 'lola) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 'lol)", Atom "foo"),
          ("(eq? foo 'lola)", ValBool False)
        ]

    it "(define foo 'lola) && (eq? 'lol foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 'lola)", Atom "foo"),
          ("(eq? 'lol foo)", ValBool False)
        ]

    it "(define foo 'lol) && (define bar 'lola) && (eq? foo bar) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 'lol)", Atom "foo"),
          ("(define bar 'lola)", Atom "bar"),
          ("(eq? foo bar)", ValBool False)
        ]

    it "(define foo 'lola) && (define bar 'lol) && (eq? foo bar) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 'lola)", Atom "foo"),
          ("(define bar 'lol)", Atom "bar"),
          ("(eq? foo bar)", ValBool False)
        ]

    it "(define foo 'lol) && (define bar 'lola) && (eq? bar foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 'lol)", Atom "foo"),
          ("(define bar 'lola)", Atom "bar"),
          ("(eq? bar foo)", ValBool False)
        ]

    it "(define foo 'lola) && (define bar 'lol) && (eq? bar foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 'lola)", Atom "foo"),
          ("(define bar 'lol)", Atom "bar"),
          ("(eq? bar foo)", ValBool False)
        ]

    it "(define foo 'a) && (define foo 'lola) && (define bar 'lol) && (eq? bar foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 'a)", Atom "foo"),
          ("(define foo 'lola)", Atom "foo"),
          ("(define bar 'lol)", Atom "bar"),
          ("(eq? bar foo)", ValBool False)
        ]

    it "(define bar 'a) && (define foo 'lola) && (define bar 'lol) && (eq? bar foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define bar 'a)", Atom "bar"),
          ("(define foo 'lola)", Atom "foo"),
          ("(define bar 'lol)", Atom "bar"),
          ("(eq? bar foo)", ValBool False)
        ]

    it "(define foo 'a) && (define bar 'a) && (define foo 'lola) && (define bar 'lol) && (eq? bar foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 'a)", Atom "foo"),
          ("(define bar 'a)", Atom "bar"),
          ("(define foo 'lola)", Atom "foo"),
          ("(define bar 'lol)", Atom "bar"),
          ("(eq? bar foo)", ValBool False)
        ]

    it "(eq? \"lol\" \"lol\") \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [("(eq? \"lol\" \"lol\")", ValBool True)]

    it "(define foo \"lol\") && (eq? foo \"lol\") \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo \"lol\")", Atom "foo"),
          ("(eq? foo \"lol\")", ValBool True)
        ]

    it "(define foo \"lol\") && (eq? \"lol\" foo) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo \"lol\")", Atom "foo"),
          ("(eq? \"lol\" foo)", ValBool True)
        ]

    it "(define foo \"lol\") && (define bar \"lol\") && (eq? foo bar) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo \"lol\")", Atom "foo"),
          ("(define bar \"lol\")", Atom "bar"),
          ("(eq? foo bar)", ValBool True)
        ]

    it "(define foo \"lol\") && (define bar \"lol\") && (eq? bar foo) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo \"lol\")", Atom "foo"),
          ("(define bar \"lol\")", Atom "bar"),
          ("(eq? bar foo)", ValBool True)
        ]

    it "(eq? \"lol\" \"lola\") \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [("(eq? \"lol\" \"lola\")", ValBool False)]

    it "(define foo \"lol\") && (eq? foo \"lola\") \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo \"lol\")", Atom "foo"),
          ("(eq? foo \"lola\")", ValBool False)
        ]

    it "(define foo \"lola\") && (eq? \"lol\" foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo \"lola\")", Atom "foo"),
          ("(eq? \"lol\" foo)", ValBool False)
        ]

    it "(define foo \"lol\") && (define bar \"lola\") && (eq? foo bar) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo \"lol\")", Atom "foo"),
          ("(define bar \"lola\")", Atom "bar"),
          ("(eq? foo bar)", ValBool False)
        ]

    it "(define foo \"lola\") && (define bar \"lol\") && (eq? foo bar) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo \"lola\")", Atom "foo"),
          ("(define bar \"lol\")", Atom "bar"),
          ("(eq? foo bar)", ValBool False)
        ]

    it "(define foo \"lol\") && (define bar \"lola\") && (eq? bar foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo \"lol\")", Atom "foo"),
          ("(define bar \"lola\")", Atom "bar"),
          ("(eq? bar foo)", ValBool False)
        ]

    it "(define foo \"lola\") && (define bar \"lol\") && (eq? bar foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo \"lola\")", Atom "foo"),
          ("(define bar \"lol\")", Atom "bar"),
          ("(eq? bar foo)", ValBool False)
        ]

    it "(define foo 'a) && (define foo \"lola\") && (define bar \"lol\") && (eq? bar foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 'a)", Atom "foo"),
          ("(define foo \"lola\")", Atom "foo"),
          ("(define bar \"lol\")", Atom "bar"),
          ("(eq? bar foo)", ValBool False)
        ]

    it "(define bar 'a) && (define foo \"lola\") && (define bar \"lol\") && (eq? bar foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define bar 'a)", Atom "bar"),
          ("(define foo \"lola\")", Atom "foo"),
          ("(define bar \"lol\")", Atom "bar"),
          ("(eq? bar foo)", ValBool False)
        ]

    it "(define foo 'a) && (define bar 'a) && (define foo \"lola\") && (define bar \"lol\") && (eq? bar foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 'a)", Atom "foo"),
          ("(define bar 'a)", Atom "bar"),
          ("(define foo \"lola\")", Atom "foo"),
          ("(define bar \"lol\")", Atom "bar"),
          ("(eq? bar foo)", ValBool False)
        ]

    it "(eq? \"coucou\" \"coucou\") \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [("(eq? \"coucou\" \"coucou\")", ValBool True)]

    it "(define foo \"coucou\") && (eq? foo \"coucou\") \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo \"coucou\")", Atom "foo"),
          ("(eq? foo \"coucou\")", ValBool True)
        ]

    it "(define foo \"coucou\") && (eq? \"coucou\" foo) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo \"coucou\")", Atom "foo"),
          ("(eq? \"coucou\" foo)", ValBool True)
        ]

    it "(define foo \"coucou\") && (define bar \"coucou\") && (eq? foo bar) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo \"coucou\")", Atom "foo"),
          ("(define bar \"coucou\")", Atom "bar"),
          ("(eq? foo bar)", ValBool True)
        ]

    it "(define foo \"coucou\") && (define bar \"coucou\") && (eq? bar foo) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo \"coucou\")", Atom "foo"),
          ("(define bar \"coucou\")", Atom "bar"),
          ("(eq? bar foo)", ValBool True)
        ]

    it "(eq? \"coucou\" \"bonjour\") \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [("(eq? \"coucou\" \"bonjour\")", ValBool False)]

    it "(define foo \"coucou\") && (eq? foo \"bonjour\") \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo \"coucou\")", Atom "foo"),
          ("(eq? foo \"bonjour\")", ValBool False)
        ]

    it "(define foo \"bonjour\") && (eq? \"coucou\" foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo \"bonjour\")", Atom "foo"),
          ("(eq? \"coucou\" foo)", ValBool False)
        ]

    it "(define foo \"coucou\") && (define bar \"bonjour\") && (eq? foo bar) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo \"coucou\")", Atom "foo"),
          ("(define bar \"bonjour\")", Atom "bar"),
          ("(eq? foo bar)", ValBool False)
        ]

    it "(define foo \"bonjour\") && (define bar \"coucou\") && (eq? foo bar) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo \"bonjour\")", Atom "foo"),
          ("(define bar \"coucou\")", Atom "bar"),
          ("(eq? foo bar)", ValBool False)
        ]

    it "(define foo \"coucou\") && (define bar \"bonjour\") && (eq? bar foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo \"coucou\")", Atom "foo"),
          ("(define bar \"bonjour\")", Atom "bar"),
          ("(eq? bar foo)", ValBool False)
        ]

    it "(define foo \"bonjour\") && (define bar \"coucou\") && (eq? bar foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo \"bonjour\")", Atom "foo"),
          ("(define bar \"coucou\")", Atom "bar"),
          ("(eq? bar foo)", ValBool False)
        ]

    it "(define foo 'a) && (define foo \"bonjour\") && (define bar \"coucou\") && (eq? bar foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 'a)", Atom "foo"),
          ("(define foo \"bonjour\")", Atom "foo"),
          ("(define bar \"coucou\")", Atom "bar"),
          ("(eq? bar foo)", ValBool False)
        ]

    it "(define bar 'a) && (define foo \"bonjour\") && (define bar \"coucou\") && (eq? bar foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define bar 'a)", Atom "bar"),
          ("(define foo \"bonjour\")", Atom "foo"),
          ("(define bar \"coucou\")", Atom "bar"),
          ("(eq? bar foo)", ValBool False)
        ]

    it "(define foo 'a) && (define bar 'a) && (define foo \"bonjour\") && (define bar \"coucou\") && (eq? bar foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 'a)", Atom "foo"),
          ("(define bar 'a)", Atom "bar"),
          ("(define foo \"bonjour\")", Atom "foo"),
          ("(define bar \"coucou\")", Atom "bar"),
          ("(eq? bar foo)", ValBool False)
        ]

testIfs :: Spec
testIfs =
  describe "Tests for if condition then validated else other" $ do
    it "(if (> 2 2) #t #f) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(if (> 2 2) #t #f)", Atom "#f")
        ]

    it "(define foo 2) && (if (> foo 2) #t #f) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 2)", Atom "foo"),
          ("(if (> foo 2) #t #f)", Atom "#f")
        ]

    it "(define foo 2) && (if (> 2 foo) #t #f) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 2)", Atom "foo"),
          ("(if (> 2 foo) #t #f)", Atom "#f")
        ]


    it "(if (> 2 2) 1 2) \t->\t 2" $ do
      chainAssertion
        emptyEnv
        [ ("(if (> 2 2) 1 2)", ValNum 2)
        ]

    it "(define foo 2) && (if (> foo 2) 1 2) \t->\t 2" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 2)", Atom "foo"),
          ("(if (> foo 2) 1 2)", ValNum 2)
        ]

    it "(define foo 2) && (if (> 2 foo) 1 2) \t->\t 2" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 2)", Atom "foo"),
          ("(if (> 2 foo) 1 2)", ValNum 2)
        ]

    it "(define foo 2) && (if (> 2 2) 1 foo) \t->\t 2" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 2)", Atom "foo"),
          ("(if (> 2 2) 1 foo)", ValNum 2)
        ]

    it "(define foo 1) && (if (> 2 2) foo 2) \t->\t 1" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 1)", Atom "foo"),
          ("(if (> 2 2) foo 2)", ValNum 1)
        ]

----

    it "(if (> 2 2) 2 1) \t->\t 2" $ do
      chainAssertion
        emptyEnv
        [ ("(if (> 2 2) 2 1)", ValNum 1)
        ]

    it "(if (> 2 2) 'lol 'mdr) \t->\t mdr" $ do
      chainAssertion
        emptyEnv
        [ ("(if (> 2 2) 'lol 'mdr)", Atom "mdr")
        ]

    it "(if (> 2 2) 'mdr 'lol) \t->\t lol" $ do
      chainAssertion
        emptyEnv
        [ ("(if (> 2 2) 'mdr 'lol)", Atom "lol")
        ]

    it "(if (> 2 2) \"lol\" \"mdr\") \t->\t \"mdr\"" $ do
      chainAssertion
        emptyEnv
        [ ("(if (> 2 2) \"lol\" \"mdr\")", ValString "mdr")
        ]

    it "(if (> 2 2) \"mdr\" \"lol\") \t->\t \"lol\"" $ do
      chainAssertion
        emptyEnv
        [ ("(if (> 2 2) \"mdr\" \"lol\")", ValString "lol")
        ]


    it "(if (< 2 2) #t #f) \t->\t #f" $ do
      unpackError (parseExpr emptyEnv "(if (< 2 2) #t #f)") `shouldSatisfy` expectRightValue (Atom "#f")

    it "(if (!= 2 2) #t #f) \t->\t #f" $ do
      unpackError (parseExpr emptyEnv "(if (!= 2 2) #t #f)") `shouldSatisfy` expectRightValue (Atom "#f")

    it "(if (>= 2 2) #t #f) \t->\t #t" $ do
      unpackError (parseExpr emptyEnv "(if (>= 2 2) #t #f)") `shouldSatisfy` expectRightValue (Atom "#t")

    it "(if (<= 2 2) #t #f) \t->\t #t" $ do
      unpackError (parseExpr emptyEnv "(if (<= 2 2) #t #f)") `shouldSatisfy` expectRightValue (Atom "#t")

    it "(if (= 2 2) #t #f) \t->\t #t" $ do
      unpackError (parseExpr emptyEnv "(if (= 2 2) #t #f)") `shouldSatisfy` expectRightValue (Atom "#t")

    it "(if (eq? 2 2) #t #f) \t->\t #t" $ do
      unpackError (parseExpr emptyEnv "(if (eq? 2 2) #t #f)") `shouldSatisfy` expectRightValue (Atom "#t")

    it "(if (eq? 'foo 'foo) #t #f) \t->\t #t" $ do
      unpackError (parseExpr emptyEnv "(if (eq? 'foo 'foo) #t #f)") `shouldSatisfy` expectRightValue (Atom "#t")

    it "(if (eq? \"lol\" \"lol\") #t #f) \t->\t #t" $ do
      unpackError (parseExpr emptyEnv "(if (eq? 'foo 'foo) #t #f)") `shouldSatisfy` expectRightValue (Atom "#t")

    it "(if (eq? \"lola\" \"lol\") #t #f) \t->\t #t" $ do
      unpackError (parseExpr emptyEnv "(if (eq? \"lola\" \"lol\") #t #f)") `shouldSatisfy` expectRightValue (Atom "#f")

    it "(if (eq? \"lol\" \"lola\") #t #f) \t->\t #t" $ do
      unpackError (parseExpr emptyEnv "(if (eq? \"lol\" \"lola\") #t #f)") `shouldSatisfy` expectRightValue (Atom "#f")

    it "(if (eq? 'foo 'foob) #t #f) \t->\t #f" $ do
      unpackError (parseExpr emptyEnv "(if (eq? 'foo 'foob) #t #f)") `shouldSatisfy` expectRightValue (Atom "#f")

    it "(if (eq? #t #t) #t #f) \t->\t #t" $ do
      unpackError (parseExpr emptyEnv "(if (eq? #t #t) #t #f)") `shouldSatisfy` expectRightValue (Atom "#t")

    it "(if (eq? #f #f) #t #f) \t->\t #t" $ do
      unpackError (parseExpr emptyEnv "(if (eq? #f #f) #t #f)") `shouldSatisfy` expectRightValue (Atom "#t")

    it "(if (eq? #t #f) #t #f) \t->\t #f" $ do
      unpackError (parseExpr emptyEnv "(if (eq? #t #f) #t #f)") `shouldSatisfy` expectRightValue (Atom "#f")

    it "(if (eq? #f #t) #t #f) \t->\t #f" $ do
      unpackError (parseExpr emptyEnv "(if (eq? #f #t) #t #f)") `shouldSatisfy` expectRightValue (Atom "#f")

    it "(if (> 2 2) 1 2) \t->\t 2" $ do
      unpackError (parseExpr emptyEnv "(if (> 2 2) 1 2)") `shouldSatisfy` expectRightValue (ValNum 2)

    it "(if (< 2 2) 1 2) \t->\t 2" $ do
      unpackError (parseExpr emptyEnv "(if (< 2 2) 1 2)") `shouldSatisfy` expectRightValue (ValNum 2)

    it "(if (!= 2 2) 1 2) \t->\t 2" $ do
      unpackError (parseExpr emptyEnv "(if (!= 2 2) 1 2)") `shouldSatisfy` expectRightValue (ValNum 2)

    it "(if (>= 2 2) 1 2) \t->\t 1" $ do
      unpackError (parseExpr emptyEnv "(if (>= 2 2) 1 2)") `shouldSatisfy` expectRightValue (ValNum 1)

    it "(if (<= 2 2) 1 2) \t->\t 1" $ do
      unpackError (parseExpr emptyEnv "(if (<= 2 2) 1 2)") `shouldSatisfy` expectRightValue (ValNum 1)

    it "(if (= 2 2) 1 2) \t->\t 1" $ do
      unpackError (parseExpr emptyEnv "(if (= 2 2) 1 2)") `shouldSatisfy` expectRightValue (ValNum 1)

    it "(if (eq? 2 2) 1 2) \t->\t 1" $ do
      unpackError (parseExpr emptyEnv "(if (eq? 2 2) 1 2)") `shouldSatisfy` expectRightValue (ValNum 1)

    it "(if (eq? 'foo 'foo) 1 2) \t->\t 1" $ do
      unpackError (parseExpr emptyEnv "(if (eq? 'foo 'foo) 1 2)") `shouldSatisfy` expectRightValue (ValNum 1)

    it "(if (eq? 'foo 'foob) 1 2) \t->\t 2" $ do
      unpackError (parseExpr emptyEnv "(if (eq? 'foo 'foob) 1 2)") `shouldSatisfy` expectRightValue (ValNum 2)

    it "(if (eq? 1 1) 1 2) \t->\t 1" $ do
      unpackError (parseExpr emptyEnv "(if (eq? 1 1) 1 2)") `shouldSatisfy` expectRightValue (ValNum 1)

    it "(if (eq? 2 2) 1 2) \t->\t 1" $ do
      unpackError (parseExpr emptyEnv "(if (eq? 2 2) 1 2)") `shouldSatisfy` expectRightValue (ValNum 1)

    it "(if (eq? 1 2) 1 2) \t->\t 2" $ do
      unpackError (parseExpr emptyEnv "(if (eq? 1 2) 1 2)") `shouldSatisfy` expectRightValue (ValNum 2)

    it "(if (eq? 2 1) 1 2) \t->\t 2" $ do
      unpackError (parseExpr emptyEnv "(if (eq? 2 1) 1 2)") `shouldSatisfy` expectRightValue (ValNum 2)

    it "(define foo \"lol\") && (if (eq? foo \"lol\") #t #f) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo \"lol\")", Atom "foo"),
          ("(if (eq? foo \"lol\") #t #f)", Atom "#t")
        ]

    it "(define foo \"lola\") && (if (eq? foo \"lol\") #t #f) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo \"lola\")", Atom "foo"),
          ("(if (eq? foo \"lol\") #t #f)", Atom "#f")
        ]

testArithmetic :: Spec
testArithmetic =
  describe "Tests for Arithmetic" $ do
    it "(+ 2 2) \t->\t 4" $ do
      chainAssertion
        emptyEnv
        [("(+ 2 2)", ValNum 4)]
    it "(+ 2 -2) \t->\t 0" $ do
      chainAssertion
        emptyEnv
        [("(+ 2 -2)", ValNum 0)]
    it "(+ 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2) \t->\t 48" $ do
      chainAssertion
        emptyEnv
        [("(+ 2 2 2 2 2 2 2 2 2 2 2 2 2 2 -2 2 2 2 2 2 2 2 2 2 2 2)", ValNum 48)]
    it "(- 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2) \t->\t -48" $ do
      chainAssertion
        emptyEnv
        [("(- 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)", ValNum (-48))]
    it "(- 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 -2 2 2 2 2 2 2 2 2 2) \t->\t -48" $ do
      chainAssertion
        emptyEnv
        [("(- 2 2 2 2 2 2 2 2 2 2 2 2 2 2 -2 2 2 2 2 2 2 2 2 2 2 2)", ValNum (-44))]
    it "(+ 123 1231 23 213 123 123 12 31 23 123 123 1 23 123 123 12 31 23 123 4 23 453 453 45 34 343 534 5) \t->\t 4255" $ do
      chainAssertion
        emptyEnv
        [("(+ 123 1231 23 213 123 123 12 31 23 123 -123 1 23 123 123 12 31 23 123 4 23 453 453 45 34 343 534 5)", ValNum 4255)]
    it "(- 123 1231 23 213 123 123 12 31 23 123 123 1 23 123 123 12 31 23 123 4 23 453 453 45 34 343 534 5) \t->\t -4255" $ do
      chainAssertion
        emptyEnv
        [("(- 123 1231 23 213 123 123 12 31 23 123 123 1 23 123 123 12 31 23 123 4 23 453 453 45 34 343 534 5)", ValNum (-4255))]

    it "(* 2 2) \t->\t 4" $ do
      chainAssertion
        emptyEnv
        [("(* 2 2)", ValNum 4)]
    it "(* 5 5) \t->\t 4" $ do
      chainAssertion
        emptyEnv
        [("(* 5 5)", ValNum 25)]
    it "(* 25 25) \t->\t 4" $ do
      chainAssertion
        emptyEnv
        [("(* 25 25)", ValNum 625)]
    it "(* 2 -2) \t->\t 0" $ do
      chainAssertion
        emptyEnv
        [("(* 2 -2)", ValNum (-4))]
    it "(* 5 -5) \t->\t 4" $ do
      chainAssertion
        emptyEnv
        [("(* 5 -5)", ValNum (-25))]
    it "(* 25 -25) \t->\t 4" $ do
      chainAssertion
        emptyEnv
        [("(* 25 -25)", ValNum (-625))]
    it "(* 2 -2) \t->\t 0" $ do
      chainAssertion
        emptyEnv
        [("(* 2 -2)", ValNum (-4))]
    it "(* 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2) \t->\t 67108864" $ do
      chainAssertion
        emptyEnv
        [("(* 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)", ValNum 67108864)]
    it "(* -2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2) \t->\t -67108864" $ do
      chainAssertion
        emptyEnv
        [("(* -2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)", ValNum (-67108864))]
    it "(* 2 2 -2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2) \t->\t -67108864" $ do
      chainAssertion
        emptyEnv
        [("(* 2 2 -2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)", ValNum (-67108864))]
    it "(* 2 2 2 2 2 2 2 -2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2) \t->\t -67108864" $ do
      chainAssertion
        emptyEnv
        [("(* 2 2 2 2 2 2 2 -2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)", ValNum (-67108864))]
    it "(* 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 -2 2 2 2 2 2 2 2 2 2 2) \t->\t -67108864" $ do
      chainAssertion
        emptyEnv
        [("(* 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 -2 2 2 2 2 2 2 2 2 2 2)", ValNum (-67108864))]
    it "(* 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 -2) \t->\t -67108864" $ do
      chainAssertion
        emptyEnv
        [("(* 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 -2)", ValNum (-67108864))]
    it "(* 123 23 123 123 12 31 23 123 123 1 23 123 123 12) \t->\t 23133613427319697951536" $ do
      chainAssertion
        emptyEnv
        [("(* 123 23 123 123 12 31 23 123 123 1 23 123 123 12)", ValNum (23133613427319697951536))]
    it "(* -123 23 123 123 12 31 23 123 123 1 23 123 123 12) \t->\t -23133613427319697951536" $ do
      chainAssertion
        emptyEnv
        [("(* -123 23 123 123 12 31 23 123 123 1 23 123 123 12)", ValNum (-23133613427319697951536))]
    it "(* 123 23 123 -123 12 31 23 123 123 1 23 123 123 12) \t->\t -23133613427319697951536" $ do
      chainAssertion
        emptyEnv
        [("(* 123 23 123 -123 12 31 23 123 123 1 23 123 123 12)", ValNum (-23133613427319697951536))]
    it "(* 123 23 123 123 12 31 -23 123 123 1 23 123 123 12) \t->\t -23133613427319697951536" $ do
      chainAssertion
        emptyEnv
        [("(* 123 23 123 123 12 31 -23 123 123 1 23 123 123 12)", ValNum (-23133613427319697951536))]
    it "(* 123 23 123 123 12 31 23 123 123 -1 23 123 123 12) \t->\t -23133613427319697951536" $ do
      chainAssertion
        emptyEnv
        [("(* 123 23 123 123 12 31 23 123 123 -1 23 123 123 12)", ValNum (-23133613427319697951536))]
    it "(* 123 23 123 123 12 31 23 123 123 1 23 123 123 -12) \t->\t -23133613427319697951536" $ do
      chainAssertion
        emptyEnv
        [("(* 123 23 123 123 12 31 23 123 123 1 23 123 123 -12)", ValNum (-23133613427319697951536))]

    it "(* -2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 -2) \t->\t 67108864" $ do
      chainAssertion
        emptyEnv
        [("(* -2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 -2)", ValNum 67108864)]
    it "(* -2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 -2 2 2 2 2 2 2) \t->\t 67108864" $ do
      chainAssertion
        emptyEnv
        [("(* -2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 -2 2 2 2 2 2 2 2 2 2 2)", ValNum (67108864))]
    it "(* 2 2 -2 2 2 2 2 2 2 2 2 2 2 2 2 2 -2 2 2 2 2 2 2 2 2 2) \t->\t 67108864" $ do
      chainAssertion
        emptyEnv
        [("(* 2 2 -2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 -2 2 2 2 2 2 2 2 2)", ValNum (67108864))]
    it "(* 2 2 2 2 2 2 2 -2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 -2) \t->\t 67108864" $ do
      chainAssertion
        emptyEnv
        [("(* 2 2 2 2 2 2 2 -2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 -2)", ValNum (67108864))]
    it "(* 2 2 2 -2 2 2 2 2 2 2 2 2 2 2 2 -2 2 2 2 2 2 2 2 2 2 2) \t->\t 67108864" $ do
      chainAssertion
        emptyEnv
        [("(* 2 2 2 -2 2 2 2 2 2 2 2 2 2 2 2 -2 2 2 2 2 2 2 2 2 2 2)", ValNum (67108864))]
    it "(* 2 -2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 -2) \t->\t 67108864" $ do
      chainAssertion
        emptyEnv
        [("(* 2 -2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 -2)", ValNum (67108864))]
    it "(* -123 23 123 123 12 31 23 123 123 1 23 123 123 -12) \t->\t 23133613427319697951536" $ do
      chainAssertion
        emptyEnv
        [("(* -123 23 123 123 12 31 23 123 123 1 23 123 123 -12)", ValNum (23133613427319697951536))]
    it "(* -123 23 -123 123 12 31 23 123 123 1 23 123 123 12) \t->\t 23133613427319697951536" $ do
      chainAssertion
        emptyEnv
        [("(* -123 23 -123 123 12 31 23 123 123 1 23 123 123 12)", ValNum (23133613427319697951536))]
    it "(* 123 23 123 -123 -12 31 23 123 123 1 23 123 123 12) \t->\t 23133613427319697951536" $ do
      chainAssertion
        emptyEnv
        [("(* 123 23 123 -123 -12 31 23 123 123 1 23 123 123 12)", ValNum (23133613427319697951536))]
    it "(* 123 23 123 123 12 -31 -23 123 123 1 23 123 123 12) \t->\t 23133613427319697951536" $ do
      chainAssertion
        emptyEnv
        [("(* 123 23 123 123 12 -31 -23 123 123 1 23 123 123 12)", ValNum (23133613427319697951536))]
    it "(* 123 23 123 -123 12 31 23 123 123 -1 23 123 123 12) \t->\t 23133613427319697951536" $ do
      chainAssertion
        emptyEnv
        [("(* 123 23 123 -123 12 31 23 123 123 -1 23 123 123 12)", ValNum (23133613427319697951536))]
    it "(* 123 23 123 -123 12 31 23 123 123 1 23 123 123 -12) \t->\t 23133613427319697951536" $ do
      chainAssertion
        emptyEnv
        [("(* 123 23 123 -123 12 31 23 123 123 1 23 123 123 -12)", ValNum (23133613427319697951536))]

    it "(= 2 2) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [("(= 2 2)", ValBool True)]
    it "(= 2 3) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [("(= 2 3)", ValBool False)]
    it "(!= 2 2) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [("(!= 2 2)", ValBool False)]
    it "(!= 2 3) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [("(!= 2 3)", ValBool True)]
    it "(define foo 2) && (= foo 2) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 2)", Atom "foo"),
          ("(= foo 2)", ValBool True)
        ]
    it "(define foo 2) && (= 2 3) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 2)", Atom "foo"),
          ("(= foo 3)", ValBool False)
        ]
    it "(define foo 2) && (!= 2 2) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 2)", Atom "foo"),
          ("(!= foo 2)", ValBool False)
        ]
    it "(define foo 2) && (!= 2 3) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 2)", Atom "foo"),
          ("(!= foo 3)", ValBool True)
        ]
    it "(define foo 2) && (define bar 2) && (= foo bar) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 2)", Atom "foo"),
          ("(define bar 2)", Atom "bar"),
          ("(= foo bar)", ValBool True)
        ]
    it "(define foo 2) && (define bar 3) && (= 2 3) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 2)", Atom "foo"),
          ("(define bar 3)", Atom "bar"),
          ("(= foo bar)", ValBool False)
        ]
    it "(define foo 2) && (define bar 2) && (!= 2 2) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 2)", Atom "foo"),
          ("(define bar 2)", Atom "bar"),
          ("(!= foo bar)", ValBool False)
        ]
    it "(define foo 2) && (define bar 3) && (!= 2 3) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 2)", Atom "foo"),
          ("(define bar 3)", Atom "bar"),
          ("(!= foo bar)", ValBool True)
        ]

    it "(> 2 2) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(> 2 2)", ValBool False)
        ]
    it "(< 2 2) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(< 2 2)", ValBool False)
        ]
    it "(>= 2 2) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(>= 2 2)", ValBool True)
        ]
    it "(<= 2 2) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(<= 2 2)", ValBool True)
        ]
    it "(define foo 2) && (> foo 2) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 2)", Atom "foo"),
          ("(> foo 2)", ValBool False)
        ]
    it "(define foo 2) && (< foo 2) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 2)", Atom "foo"),
          ("(< foo 2)", ValBool False)
        ]
    it "(define foo 2) && (>= foo 2) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 2)", Atom "foo"),
          ("(>= foo 2)", ValBool True)
        ]
    it "(define foo 2) && (> 2 foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 2)", Atom "foo"),
          ("(> 2 foo)", ValBool False)
        ]
    it "(define foo 2) && (< 2 foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 2)", Atom "foo"),
          ("(< 2 foo)", ValBool False)
        ]
    it "(define foo 2) && (< 2 foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 2)", Atom "foo"),
          ("(< 2 foo)", ValBool False)
        ]
    it "(define foo 2) && (>= 2 foo) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 2)", Atom "foo"),
          ("(>= 2 foo)", ValBool True)
        ]
    it "(define foo 2) && (<= foo 2) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 2)", Atom "foo"),
          ("(<= foo 2)", ValBool True)
        ]
    it "(define foo 2) && (define bar 2) && (> foo bar) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 2)", Atom "foo"),
          ("(define bar 2)", Atom "bar"),
          ("(> foo bar)", ValBool False)
        ]
    it "(define foo 2) && (define bar 2) && (< foo bar) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 2)", Atom "foo"),
          ("(define bar 2)", Atom "bar"),
          ("(< foo bar)", ValBool False)
        ]
    it "(define foo 2) && (define bar 2) && (>= foo bar) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 2)", Atom "foo"),
          ("(define bar 2)", Atom "bar"),
          ("(>= foo bar)", ValBool True)
        ]
    it "(define foo 2) && (define bar 2) && (<= foo bar) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 2)", Atom "foo"),
          ("(define bar 2)", Atom "bar"),
          ("(<= foo bar)", ValBool True)
        ]

    it "(> 2 3) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(> 2 3)", ValBool False)
        ]
    it "(< 2 3) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(< 2 3)", ValBool True)
        ]
    it "(>= 2 3) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(>= 2 3)", ValBool False)
        ]
    it "(<= 2 3) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(<= 2 3)", ValBool True)
        ]
    it "(define foo 2) && (> foo 3) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 2)", Atom "foo"),
          ("(> foo 3)", ValBool False)
        ]
    it "(define foo 2) && (< foo 3) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 2)", Atom "foo"),
          ("(< foo 3)", ValBool True)
        ]
    it "(define foo 3) && (> 2 foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 3)", Atom "foo"),
          ("(> 2 foo)", ValBool False)
        ]
    it "(define foo 3) && (< 2 foo) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 3)", Atom "foo"),
          ("(< 2 foo)", ValBool True)
        ]
    it "(define foo 2) && (>= foo 3) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 2)", Atom "foo"),
          ("(>= foo 3)", ValBool False)
        ]
    it "(define foo 2) && (<= foo 2) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 2)", Atom "foo"),
          ("(<= foo 2)", ValBool True)
        ]
    it "(define foo 3) && (>= 2 foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 3)", Atom "foo"),
          ("(>= 2 foo)", ValBool False)
        ]
    it "(define foo 3) && (<= 2 foo) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 3)", Atom "foo"),
          ("(<= 2 foo)", ValBool True)
        ]
    it "(define foo 2) && (define bar 3) && (> foo bar) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 2)", Atom "foo"),
          ("(define bar 3)", Atom "bar"),
          ("(> foo bar)", ValBool False)
        ]
    it "(define foo 2) && (define bar 3) && (< foo bar) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 2)", Atom "foo"),
          ("(define bar 3)", Atom "bar"),
          ("(< foo bar)", ValBool True)
        ]
    it "(define foo 2) && (define bar 3) && (>= foo bar) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 2)", Atom "foo"),
          ("(define bar 3)", Atom "bar"),
          ("(>= foo bar)", ValBool False)
        ]
    it "(define foo 2) && (define bar 3) && (<= foo bar) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 2)", Atom "foo"),
          ("(define bar 3)", Atom "bar"),
          ("(<= foo bar)", ValBool True)
        ]

    it "(> 3 2) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(> 3 2)", ValBool True)
        ]
    it "(< 3 2) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(< 3 2)", ValBool False)
        ]
    it "(>= 3 2) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(>= 3 2)", ValBool True)
        ]
    it "(<= 3 2) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(<= 3 2)", ValBool False)
        ]
    it "(define foo 3) && (> foo 2) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 3)", Atom "foo"),
          ("(> foo 2)", ValBool True)
        ]
    it "(define foo 3) && (< foo 2) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 3)", Atom "foo"),
          ("(< foo 2)", ValBool False)
        ]
    it "(define foo 2) && (> 3 foo) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 2)", Atom "foo"),
          ("(> 3 foo)", ValBool True)
        ]
    it "(define foo 2) && (< 3 foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 2)", Atom "foo"),
          ("(< 3 foo)", ValBool False)
        ]
    it "(define foo 3) && (>= foo 2) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 3)", Atom "foo"),
          ("(>= foo 2)", ValBool True)
        ]
    it "(define foo 3) && (<= foo 3) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 3)", Atom "foo"),
          ("(<= foo 3)", ValBool True)
        ]
    it "(define foo 2) && (>= 3 foo) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 2)", Atom "foo"),
          ("(>= 3 foo)", ValBool True)
        ]
    it "(define foo 2) && (<= 3 foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 2)", Atom "foo"),
          ("(<= 3 foo)", ValBool False)
        ]
    it "(define foo 3) && (define bar 2) && (> foo bar) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 3)", Atom "foo"),
          ("(define bar 2)", Atom "bar"),
          ("(> foo bar)", ValBool True)
        ]
    it "(define foo 3) && (define bar 2) && (< foo bar) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 3)", Atom "foo"),
          ("(define bar 2)", Atom "bar"),
          ("(< foo bar)", ValBool False)
        ]
    it "(define foo 3) && (define bar 2) && (>= foo bar) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 3)", Atom "foo"),
          ("(define bar 2)", Atom "bar"),
          ("(>= foo bar)", ValBool True)
        ]
    it "(define foo 3) && (define bar 2) && (<= foo bar) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 3)", Atom "foo"),
          ("(define bar 2)", Atom "bar"),
          ("(<= foo bar)", ValBool False)
        ]

testsLetStatement :: Spec
testsLetStatement =
  describe "Tests for let" $ do
    it "(eq? 2 2) \t->\t #t" $ do
      pendingWith ""

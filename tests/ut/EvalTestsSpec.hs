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
      unpackError (parseExpr emptyEnv "(eq? 2 2)") `shouldSatisfy` expectRightValue (ValBool True)

    it "(eq? 2 3) \t->\t #t" $ do
      unpackError (parseExpr emptyEnv "(eq? 2 3)") `shouldSatisfy` expectRightValue (ValBool False)

    it "(eq? 200000000000000 200000000000000) \t->\t #t" $ do
      unpackError (parseExpr emptyEnv "(eq? 200000000000000 200000000000000)") `shouldSatisfy` expectRightValue (ValBool True)

    it "(eq? 2 3) \t->\t #t" $ do
      unpackError (parseExpr emptyEnv "(eq? 2 3)") `shouldSatisfy` expectRightValue (ValBool False)

    it "(eq? 0 0) \t->\t #t" $ do
      unpackError (parseExpr emptyEnv "(eq? 0 0)") `shouldSatisfy` expectRightValue (ValBool True)

    it "(eq? 0 3) \t->\t #t" $ do
      unpackError (parseExpr emptyEnv "(eq? 0 3)") `shouldSatisfy` expectRightValue (ValBool False)

    it "(eq? 0 0) \t->\t #t" $ do
      unpackError (parseExpr emptyEnv "(eq? 0 0)") `shouldSatisfy` expectRightValue (ValBool True)

    it "(eq? 0 3) \t->\t #t" $ do
      unpackError (parseExpr emptyEnv "(eq? 0 3)") `shouldSatisfy` expectRightValue (ValBool False)

    it "(eq? -2 -2) \t->\t #t" $ do
      unpackError (parseExpr emptyEnv "(eq? -2 -2)") `shouldSatisfy` expectRightValue (ValBool True)

    it "(eq? -2 -3) \t->\t #t" $ do
      unpackError (parseExpr emptyEnv "(eq? -2 -3)") `shouldSatisfy` expectRightValue (ValBool False)

    it "(eq? -200000000000000 -200000000000000) \t->\t #t" $ do
      unpackError (parseExpr emptyEnv "(eq? -200000000000000 -200000000000000)") `shouldSatisfy` expectRightValue (ValBool True)

    it "(eq? 'foo 'foo) \t->\t #t" $ do
      unpackError (parseExpr emptyEnv "(eq? 'foo 'foo)") `shouldSatisfy` expectRightValue (ValBool True)

    it "(eq? 'foo 'fooa) \t->\t #f" $ do
      unpackError (parseExpr emptyEnv "(eq? 'foo 'fooa)") `shouldSatisfy` expectRightValue (ValBool False)

    it "(eq? foo foo) \t->\t #t" $ do
      unpackError (parseExpr emptyEnv "(eq? 'foo 'foo)") `shouldSatisfy` expectRightValue (ValBool True)

    it "(eq? foo fooa) \t->\t #f" $ do
      unpackError (parseExpr emptyEnv "(eq? 'foo 'fooa)") `shouldSatisfy` expectRightValue (ValBool False)

    it "(define foo 42) && (eq? foo 42) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 42)", Atom "foo"),
          ("(eq? foo 42)", ValBool True)
        ]

    it "(define foo 42) && (eq? foo 43) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 42)", Atom "foo"),
          ("(eq? foo 43)", ValBool False)
        ]

    it "(define foo 42) && (eq? 42 foo) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 42)", Atom "foo"),
          ("(eq? 42 foo)", ValBool True)
        ]

    it "(define foo 42) && (eq? 43 foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo 42)", Atom "foo"),
          ("(eq? 43 foo)", ValBool False)
        ]

    it "(define foo \"lol\") && (eq? \"lol\" foo) \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo \"lol\")", Atom "foo"),
          ("(eq? foo \"lol\")", ValBool True)
        ]

    it "(define foo \"lola\") && (eq? \"lol\" foo) \t->\t #f" $ do
      chainAssertion
        emptyEnv
        [ ("(define foo \"lola\")", Atom "foo"),
          ("(eq? foo \"lol\")", ValBool False)
        ]

testIfs :: Spec
testIfs =
  describe "Tests for if condition then validated else other" $ do
    it "(if (> 2 2) #t #f) \t->\t #f" $ do
      unpackError (parseExpr emptyEnv "(if (> 2 2) #t #f)") `shouldSatisfy` expectRightValue (Atom "#f")

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

testsLetStatement :: Spec
testsLetStatement =
  describe "Tests for let" $ do
    it "(eq? 2 2) \t->\t #t" $ do
        pendingWith ""

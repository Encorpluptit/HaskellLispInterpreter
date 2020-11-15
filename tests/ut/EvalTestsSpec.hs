module EvalTestsSpec where

import Data.Either
import DataTypes
import Parser
import Test.Hspec

spec :: Spec
spec = describe "Eval Testing" $ do
  testEquals
  testIfs

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


--testParseChar :: Spec
--testParseChar =
--    describe "Parse Char" $ do
--        it "parse 'a' in \"abcd\" -> Right ('a', \"bcd\")" $ do
--            runParser (parseChar 'a') "abcd" `shouldBe` Right ('a', "bcd")
--        it "parse 'z' in \"abcd\" -> Left _" $ do
--            runParser (parseChar 'z') "abcd" `shouldSatisfy` isLeft
--        it "parse 'a' in \"aaaa\" -> Right ('a', \"aaa\")" $ do
--            runParser (parseChar 'a') "aaaa" `shouldBe` Right ('a', "aaa")
--        it "parse 'b' in \"baaa\" -> Right ('b', \"aaa\")" $ do
--            runParser (parseChar 'b') "baaa" `shouldBe` Right ('b', "aaa")
--        it "parse 'b' in \"bcda\" -> Right ('b', \"cda\")" $ do
--            runParser (parseChar 'b') "bcda" `shouldBe` Right ('b', "cda")
--        it "parse 'b' in \"abcd\" -> Left _" $ do
--            runParser (parseChar 'b') "abcd" `shouldSatisfy` isLeft

module FullProgramsSpec where

import DataTypes
import Test.Hspec
import Parser

chainAssertion :: Env -> [(String, LispVal)] -> Expectation
chainAssertion _ [] = return ()
chainAssertion env ((expr, expected) : xs) = do
  case unpackError (parseExpr env expr) of
    Left err -> expectationFailure $ "Failure in chainAssertion, got { " ++ show err ++ "}, Expected {" ++ show expected ++ " }"
    Right (result, newEnv) -> (result `shouldBe` expected) >> chainAssertion newEnv xs


spec :: Spec
spec = describe "Eval Testing" $ do
  mergeSort

mergeSort :: Spec
mergeSort =
  describe "Testing program to sort values" $ do
    it " \t->\t #t" $ do
      chainAssertion
        emptyEnv
        [ ("(define (null? l) (eq? l '()))", Atom "null?"),
          ("(define (merge-lists l1 l2) (cond((null? l1) l2) ((null? l2) l1) ((< (car l1) (car l2)) (cons (car l1) (merge-lists (cdr l1) l2))) (#t (cons (car l2) (merge-lists l1 (cdr l2))))))", Atom "merge-lists"),
          ("(define (split-half l l1 l2) (cond ((null? l) (cons l1 l2)) ((null? (cdr l)) (split-half (cdr l) (cons (car l) l1) l2)) (#t (split-half (cdr (cdr l)) (cons (car l) l1) (cons (car (cdr l)) l2)))))", Atom "split-half"),
          ("(define (merge-sort lst) (cond ((null? lst) '()) ((null? (cdr lst)) lst) (#t (let ((lsts (split-half lst '() '()))) (merge-lists (merge-sort (car lsts)) (merge-sort (cdr lsts)))))))", Atom "merge-sort"),
          ("(merge-sort '(39 16 22 24 17 29 18 26 27 3 34 25 10 6 7 12 8 30 2 21 13 36 14 38 32 41 40 4 35 19 5 33 23 9 15 31 28 20 42 37 11 1))",
          ValList [ValNum 1, ValNum 2, ValNum 3, ValNum 4, ValNum 5, ValNum 6, ValNum 7, ValNum 8, ValNum 9, ValNum 10, ValNum 11, ValNum 12, ValNum 13, ValNum 14, ValNum 15, ValNum 16, ValNum 17, ValNum 18, ValNum 19, ValNum 20, ValNum 21, ValNum 22, ValNum 23, ValNum 24, ValNum 25, ValNum 26, ValNum 27, ValNum 28, ValNum 29, ValNum 30, ValNum 31, ValNum 32, ValNum 33, ValNum 34, ValNum 35, ValNum 36, ValNum 37, ValNum 38, ValNum 39, ValNum 40, ValNum 41, ValNum 42])
        ]

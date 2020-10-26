module ParserSpec where

import Test.Hspec
import Test.QuickCheck
import Data.Either
import Parsing

spec :: Spec
spec = describe "Lib Parser Testing" $ do
    testParseChar
    testParseAnyChar
    testParseUInt
    testParseInt
    testParseUFloat
    testParseFloat
    testParseUDouble
    testParseDouble

testParseChar :: Spec
testParseChar =
    describe "Parse Char" $ do
        it "parse 'a' in \"abcd\" -> Right ('a', \"bcd\")" $ do
            runParser (parseChar 'a') "abcd" `shouldBe` Right ('a', "bcd")
        it "parse 'z' in \"abcd\" -> Left _" $ do
            runParser (parseChar 'z') "abcd" `shouldSatisfy` isLeft
        it "parse 'a' in \"aaaa\" -> Right ('a', \"aaa\")" $ do
            runParser (parseChar 'a') "aaaa" `shouldBe` Right ('a', "aaa")
        it "parse 'b' in \"baaa\" -> Right ('b', \"aaa\")" $ do
            runParser (parseChar 'b') "baaa" `shouldBe` Right ('b', "aaa")
        it "parse 'b' in \"bcda\" -> Right ('b', \"cda\")" $ do
            runParser (parseChar 'b') "bcda" `shouldBe` Right ('b', "cda")
        it "parse 'b' in \"abcd\" -> Left _" $ do
            runParser (parseChar 'b') "abcd" `shouldSatisfy` isLeft

testParseAnyChar :: Spec
testParseAnyChar =
    describe "Parse Any Char" $ do
        it "parse \"abcd\" in \"abcd\" -> Right ('a', \"bcd\")" $ do
            runParser (parseAnyChar "abcd") "abcd" `shouldBe` Right ('a', "bcd")
        it "parse \"bcda\" in \"abcd\" -> Right ('a', \"bcd\")" $ do
            runParser (parseAnyChar "bcda") "abcd" `shouldBe` Right ('a', "bcd")
        it "parse \"wxyz\" in \"abcd\" -> Left _" $ do
            runParser (parseAnyChar "wxyz") "abcd" `shouldSatisfy` isLeft
        it "parse 'z' in \"abcd\"" $ do
            runParser (parseChar 'z') "abcd" `shouldSatisfy` isLeft
        it "parse 'a' in \"aaaa\"" $ do
            runParser (parseChar 'a') "aaaa" `shouldBe` Right ('a', "aaa")
        it "parse 'b' in \"baaa\"" $ do
            runParser (parseChar 'b') "baaa" `shouldBe` Right ('b', "aaa")
        it "parse 'b' in \"bcda\"" $ do
            runParser (parseChar 'b') "bcda" `shouldBe` Right ('b', "cda")
        it "parse 'b' in \"abcd\"" $ do
            runParser (parseChar 'b') "abcd" `shouldSatisfy` isLeft

testParseUInt :: Spec
testParseUInt =
    describe "Parse Unsigned Int" $ do
        it "parse \"42aaa\" -> Right (42, \"aaa\")" $ do
            runParser parseUInt "42aaa" `shouldBe` Right (42, "aaa")
        it "parse \"4542aaa\" -> Right (4542, \"aaa\")" $ do
            runParser parseUInt "4542aaa" `shouldBe` Right (4542, "aaa")
        it "parse \"45454542aaa\" -> Right (4542, \"aaa\")" $ do
            runParser parseUInt "45454542aaa" `shouldBe` Right (45454542, "aaa")
        it "parse \"45489658962aaaa\" -> Right (4542, \"aaa\")" $ do
            runParser parseUInt "45489658962aaa" `shouldBe` Right (45489658962, "aaa")
        it "parse \"aaa\" -> Left _" $ do
            runParser parseUInt "aaa42" `shouldSatisfy` isLeft
        it "parse \"aaa42\" -> Left _" $ do
            runParser parseUInt "aaa42" `shouldSatisfy` isLeft

testParseInt :: Spec
testParseInt =
    describe "Parse Int" $ do
        it "parse \"42aaa\" -> Right (42, \"aaa\")" $ do
            runParser parseInt "42aaa" `shouldBe` Right (42, "aaa")
        it "parse \"aaa\" -> Left _" $ do
            runParser parseUInt "aaa" `shouldSatisfy` isLeft
        it "parse \"-42aaa\" -> Right (-42, \"aaa\")" $ do
            runParser parseInt "-42aaa" `shouldBe` Right (-42, "aaa")
        it "parse \"aaa\" -> Left _" $ do
            runParser parseInt "aaa" `shouldSatisfy` isLeft
        it "parse \"42aaa\" -> Right (42, \"aaa\")" $ do
            runParser parseInt "42aaa" `shouldBe` Right (42, "aaa")
        it "parse \"4542aaa\" -> Right (4542, \"aaa\")" $ do
            runParser parseInt "4542aaa" `shouldBe` Right (4542, "aaa")
        it "parse \"45454542aaa\" -> Right (45454542, \"aaa\")" $ do
            runParser parseInt "45454542aaa" `shouldBe` Right (45454542, "aaa")
        it "parse \"45489658962aaaa\" -> Right (45489658962, \"aaa\")" $ do
            runParser parseInt "45489658962aaa" `shouldBe` Right (45489658962, "aaa")
        it "parse \"aaa\" -> Left _" $ do
            runParser parseInt "aaa" `shouldSatisfy` isLeft
        it "parse \"aaa42\" -> Left _" $ do
            runParser parseInt "aaa42" `shouldSatisfy` isLeft
        it "parse \"42aaa\" -> Right (-42, \"aaa\")" $ do
            runParser parseInt "-42aaa" `shouldBe` Right (-42, "aaa")
        it "parse \"4542aaa\" -> Right (-4542, \"aaa\")" $ do
            runParser parseInt "-4542aaa" `shouldBe` Right (-4542, "aaa")
        it "parse \"45454542aaa\" -> Right (-45454542, \"aaa\")" $ do
            runParser parseInt "-45454542aaa" `shouldBe` Right (-45454542, "aaa")
        it "parse \"-45489658962aaaa\" -> Right (-45489658962, \"aaa\")" $ do
            runParser parseInt "-45489658962aaa" `shouldBe` Right (-45489658962, "aaa")
        it "parse \"aaa\" -> Left _" $ do
            runParser parseInt "aaa" `shouldSatisfy` isLeft
        it "parse \"aaa-42\" -> Left _" $ do
            runParser parseInt "aaa-42" `shouldSatisfy` isLeft

testParseUFloat :: Spec
testParseUFloat =
    describe "Parse UFloat" $ do
        it "parse \"42.8511aaa\" -> Right (-42.8511, \"aaa\")" $ do
            runParser parseFloat "42.85110aaa" `shouldBe` Right (42.8511, "aaa")
        it "parse \"42.01aaa\" -> Right (42.8511, \"aaa\")" $ do
            runParser parseUFloat "42.8511aaa" `shouldBe` Right (42.8511, "aaa")
        it "parse \"4542aaa\" -> Right (4542.8511, \"aaa\")" $ do
            runParser parseUFloat "4542.8511aaa" `shouldBe` Right (4542.8511, "aaa")
        it "parse \"45454542aaa\" -> Right (4542.8511, \"aaa\")" $ do
            runParser parseUFloat "45454542.8511aaa" `shouldBe` Right (45454542.8511, "aaa")
        it "parse \"45489658962aaaa\" -> Right (4542.8511, \"aaa\")" $ do
            runParser parseUFloat "45489658962.8511aaa" `shouldBe` Right (45489658962.8511, "aaa")
        it "parse \"aaa\" -> Left _" $ do
            runParser parseUFloat "aaa42.21585" `shouldSatisfy` isLeft
        it "parse \"aaa42.5852\" -> Left _" $ do
            runParser parseUFloat "aaa42.8511" `shouldSatisfy` isLeft

testParseFloat :: Spec
testParseFloat =
    describe "Parse Float" $ do
        it "parse \"42.8511aaa\" -> Right (42.8511, \"aaa\")" $ do
            runParser parseFloat "42.8511aaa" `shouldBe` Right (42.8511, "aaa")
        it "parse \"aaa\" -> Left _" $ do
            runParser parseFloat "aaa" `shouldSatisfy` isLeft
        it "parse \"-42.8511aaa\" -> Right (-42.8511, \"aaa\")" $ do
            runParser parseFloat "-42.8511aaa" `shouldBe` Right (-42.8511, "aaa")
        it "parse \"aaa\" -> Left _" $ do
            runParser parseFloat "aaa" `shouldSatisfy` isLeft
        it "parse \"42.8511aaa\" -> Right (42.8511, \"aaa\")" $ do
            runParser parseFloat "42.8511aaa" `shouldBe` Right (42.8511, "aaa")
        it "parse \"4542.8511aaa\" -> Right (4542.8511, \"aaa\")" $ do
            runParser parseFloat "4542.8511aaa" `shouldBe` Right (4542.8511, "aaa")
        it "parse \"45454542.8511aaa\" -> Right (45454542.8511, \"aaa\")" $ do
            runParser parseFloat "45454542.8511aaa" `shouldBe` Right (45454542.8511, "aaa")
        it "parse \"45489658962.8511aaaa\" -> Right (45489658962.8511, \"aaa\")" $ do
            runParser parseFloat "45489658962.8511aaa" `shouldBe` Right (45489658962.8511, "aaa")
        it "parse \"aaa\" -> Left _" $ do
            runParser parseFloat "aaa" `shouldSatisfy` isLeft
        it "parse \"aaa42\" -> Left _" $ do
            runParser parseFloat "aaa42.8511" `shouldSatisfy` isLeft
        it "parse \"42.8511aaa\" -> Right (-42.8511, \"aaa\")" $ do
            runParser parseFloat "-42.8511aaa" `shouldBe` Right (-42.8511, "aaa")
        it "parse \"4542.8511aaa\" -> Right (-4542.8511, \"aaa\")" $ do
            runParser parseFloat "-4542.8511aaa" `shouldBe` Right (-4542.8511, "aaa")
        it "parse \"45454542.8511aaa\" -> Right (-45454542.8511, \"aaa\")" $ do
            runParser parseFloat "-45454542.8511aaa" `shouldBe` Right (-45454542.8511, "aaa")
        it "parse \"-45489658962.8511aaaa\" -> Right (-45489658962.8511, \"aaa\")" $ do
            runParser parseFloat "-45489658962.8511aaa" `shouldBe` Right (-45489658962.8511, "aaa")
        it "parse \"aaa\" -> Left _" $ do
            runParser parseFloat "aaa" `shouldSatisfy` isLeft
        it "parse \"aaa-42.8511\" -> Left _" $ do
            runParser parseFloat "aaa-42.8511" `shouldSatisfy` isLeft

testParseUDouble :: Spec
testParseUDouble =
    describe "Parse UDouble" $ do
        it "parse \"42.8511aaa\" -> Right (-42.8511, \"aaa\")" $ do
            runParser parseDouble "42.85110aaa" `shouldBe` Right (42.8511, "aaa")
        it "parse \"42.01aaa\" -> Right (42.8511, \"aaa\")" $ do
            runParser parseUDouble "42.8511aaa" `shouldBe` Right (42.8511, "aaa")
        it "parse \"4542aaa\" -> Right (4542.8511, \"aaa\")" $ do
            runParser parseUDouble "4542.8511aaa" `shouldBe` Right (4542.8511, "aaa")
        it "parse \"45454542aaa\" -> Right (4542.8511, \"aaa\")" $ do
            runParser parseUDouble "45454542.8511aaa" `shouldBe` Right (45454542.8511, "aaa")
        it "parse \"45489658962aaaa\" -> Right (4542.8511, \"aaa\")" $ do
            runParser parseUDouble "45489658962.8511aaa" `shouldBe` Right (45489658962.8511, "aaa")
        it "parse \"aaa\" -> Left _" $ do
            runParser parseUDouble "aaa42.21585" `shouldSatisfy` isLeft
        it "parse \"aaa42.5852\" -> Left _" $ do
            runParser parseUDouble "aaa42.8511" `shouldSatisfy` isLeft

testParseDouble :: Spec
testParseDouble =
    describe "Parse Double" $ do
        it "parse \"42.8511aaa\" -> Right (42.8511, \"aaa\")" $ do
            runParser parseDouble "42.8511aaa" `shouldBe` Right (42.8511, "aaa")
        it "parse \"aaa\" -> Left _" $ do
            runParser parseDouble "aaa" `shouldSatisfy` isLeft
        it "parse \"-42.8511aaa\" -> Right (-42.8511, \"aaa\")" $ do
            runParser parseDouble "-42.8511aaa" `shouldBe` Right (-42.8511, "aaa")
        it "parse \"aaa\" -> Left _" $ do
            runParser parseDouble "aaa" `shouldSatisfy` isLeft
        it "parse \"42.8511aaa\" -> Right (42.8511, \"aaa\")" $ do
            runParser parseDouble "42.8511aaa" `shouldBe` Right (42.8511, "aaa")
        it "parse \"4542.8511aaa\" -> Right (4542.8511, \"aaa\")" $ do
            runParser parseDouble "4542.8511aaa" `shouldBe` Right (4542.8511, "aaa")
        it "parse \"45454542.8511aaa\" -> Right (45454542.8511, \"aaa\")" $ do
            runParser parseDouble "45454542.8511aaa" `shouldBe` Right (45454542.8511, "aaa")
        it "parse \"45489658962.8511aaaa\" -> Right (45489658962.8511, \"aaa\")" $ do
            runParser parseDouble "45489658962.8511aaa" `shouldBe` Right (45489658962.8511, "aaa")
        it "parse \"aaa\" -> Left _" $ do
            runParser parseDouble "aaa" `shouldSatisfy` isLeft
        it "parse \"aaa42\" -> Left _" $ do
            runParser parseDouble "aaa42.8511" `shouldSatisfy` isLeft
        it "parse \"42.8511aaa\" -> Right (-42.8511, \"aaa\")" $ do
            runParser parseDouble "-42.8511aaa" `shouldBe` Right (-42.8511, "aaa")
        it "parse \"4542.8511aaa\" -> Right (-4542.8511, \"aaa\")" $ do
            runParser parseDouble "-4542.8511aaa" `shouldBe` Right (-4542.8511, "aaa")
        it "parse \"45454542.8511aaa\" -> Right (-45454542.8511, \"aaa\")" $ do
            runParser parseDouble "-45454542.8511aaa" `shouldBe` Right (-45454542.8511, "aaa")
        it "parse \"-45489658962.8511aaaa\" -> Right (-45489658962.8511, \"aaa\")" $ do
            runParser parseDouble "-45489658962.8511aaa" `shouldBe` Right (-45489658962.8511, "aaa")
        it "parse \"aaa\" -> Left _" $ do
            runParser parseDouble "aaa" `shouldSatisfy` isLeft
        it "parse \"aaa-42.8511\" -> Left _" $ do
            runParser parseDouble "aaa-42.8511" `shouldSatisfy` isLeft

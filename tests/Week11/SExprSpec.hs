module Week11.SExprSpec where

import Data.Char
import Test.Hspec

import Week10.AParser
import Week11.SExpr

spec :: Spec
spec =
  describe "SExpr" $ do
    describe "exercise 1: parser lists" $ do
      it "zeroOrMore" $ do
        runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH" `shouldBe` Just ("ABC", "dEfgH")

      it "zeroOrMore empty" $ do
        runParser (zeroOrMore (satisfy isUpper)) "abcdeFGh" `shouldBe` Just ("", "abcdeFGh")

      it "oneOrMore" $ do
        runParser (oneOrMore (satisfy isUpper)) "ABCdEfgH" `shouldBe` Just ("ABC", "dEfgH")

      it "oneOrMore empty" $ do
        runParser (oneOrMore (satisfy isUpper)) "abcdeFGh" `shouldBe` Nothing

    describe "exercise 2: spaces" $ do
      it "test" $ do
        runParser spaces "   \t   hi" `shouldBe` Just ("   \t   ", "hi")

    describe "exercise 2: ident" $ do
      it "foobar baz" $ do
        runParser ident "foobar baz" `shouldBe` Just ("foobar", " baz")

      it "foo33fA" $ do
        runParser ident "foo33fA" `shouldBe` Just ("foo33fA", "")

      it "2bad" $ do
        runParser ident "2bad" `shouldBe` Nothing

      it "empty string" $ do
        runParser ident "" `shouldBe` Nothing

    describe "exercise 3: sexpr" $ do
      it "5" $ do
        let input = "5"
            expr  = A (N 5)
         in runParser parseSExpr input `shouldBe` Just (expr, "")

      it "foo3" $ do
        let input = "foo3"
            expr  = A (I "foo3")
         in runParser parseSExpr input `shouldBe` Just (expr, "")

      it "(bar (foo) 3 5 874)" $ do
        let input = "(bar (foo) 3 5 874)"
            expr  =
              Comb [
                A (I "bar"),
                Comb [
                  A (I "foo")
                ],
                A (N 3),
                A (N 5),
                A (N 874)
              ]
         in runParser parseSExpr input `shouldBe` Just (expr, "")

      it "(((lambda x (lambda y (plus x y))) 3) 5)" $ do
        let input = "(((lambda x (lambda y (plus x y))) 3) 5)"
            expr  =
              Comb [
                Comb [
                  Comb [
                    A (I "lambda"),
                    A (I "x"),
                    Comb [
                      A (I "lambda"),
                      A (I "y"),
                      Comb [
                        A (I "plus"),
                        A (I "x"),
                        A (I "y")
                      ]
                    ]
                  ],
                  A (N 3)
                ],
                A (N 5)
              ] 
         in runParser parseSExpr input `shouldBe` Just (expr, "")

      it "(   lots  of  (  spaces   in  )  this ( one ) )" $ do
        let input = "(   lots  of  (  spaces   in  )  this ( one ) )"
            expr  =
              Comb [
                A (I "lots"),
                A (I "of"),
                Comb [
                  A (I "spaces"),
                  A (I "in")
                ],
                A (I "this"),
                Comb [
                  A (I "one")
                ]
              ]
         in runParser parseSExpr input `shouldBe` Just (expr, "")

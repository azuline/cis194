{-# OPTIONS_GHC -Wno-type-defaults #-}

module Week10.AParserSpec where

import Control.Applicative ((<|>))
import Test.Hspec

import Week10.AParser

spec :: Spec
spec =
  describe "AParser" $ do
    describe "exercise 1: functor instance" $ do
      it "map (*2) over positive int parser" $ do
        let p = (*2) <$> posInt
         in runParser p "1234hello" `shouldBe` Just (2468, "hello")

    describe "exercise 2: applicative instance" $ do
      it "parse posInt then char" $ do
        let p = (,) <$> posInt <*> char 'h'
         in runParser p "1234hello" `shouldBe` Just ((1234, 'h'), "ello")

      it "fail parsing posInt then char 1" $ do
        let p = (,) <$> posInt <*> char 'h'
         in runParser p "x1234hello" `shouldBe` Nothing

      it "fail parsing posInt then char 2" $ do
        let p = (,) <$> posInt <*> char 'a'
         in runParser p "1234hello" `shouldBe` Nothing

    describe "exercise 3: writing parsers" $ do
      it "abParser pass" $ do
        runParser abParser "abcdef" `shouldBe` Just (('a', 'b'), "cdef")

      it "abParser fail" $ do
        runParser abParser "aebcdf" `shouldBe` Nothing

      it "abParser void output pass" $ do
        runParser abParser_ "abcdef" `shouldBe` Just ((), "cdef")

      it "abParser void output fail" $ do
        runParser abParser_ "aebcdf" `shouldBe` Nothing

      it "intPair pass" $ do
        runParser intPair "12 34" `shouldBe` Just ([12,34],"")

      it "intPair fail 1" $ do
        runParser intPair "12" `shouldBe` Nothing

      it "intPair fail 2" $ do
        runParser intPair "12c34" `shouldBe` Nothing

    describe "exercise 4: alternative instance" $ do
      it "parse either char left" $ do
        let p = char 'a' <|> char 'b'
         in runParser p "a" `shouldBe` Just ('a', "")

      it "parse either char right" $ do
        let p = char 'a' <|> char 'b'
         in runParser p "b" `shouldBe` Just ('b', "")

      it "parse either char neither" $ do
        let p = char 'a' <|> char 'b'
         in runParser p "c" `shouldBe` Nothing

    describe "exercise 5: int or uppercase" $ do
      it "342abcd" $ do
        runParser intOrUppercase "342abcd" `shouldBe` Just ((), "abcd")

      it "XYZ" $ do
        runParser intOrUppercase "XYZ" `shouldBe` Just ((), "YZ")

      it "foo" $ do
        runParser intOrUppercase "foo" `shouldBe` Nothing

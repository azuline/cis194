{-# OPTIONS_GHC -Wno-type-defaults #-}

module Week06.FibonacciSpec where

import Week06.Fibonacci
import Test.Hspec

first15Fibs :: [Integer]
first15Fibs = [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377]

first16Ruler :: [Integer]
first16Ruler = [0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4]

spec :: Spec
spec =
  describe "Fibonacci" $ do
    describe "exercise 1: recursive exponential" $ do
      it "fibs1" $ do
        take 15 fibs1 `shouldBe` first15Fibs

    describe "exercise 2: linear fibonacci" $ do
      it "fibs2" $ do
        take 15 fibs2 `shouldBe` first15Fibs

    describe "exercise 4: stream" $ do
      it "show stream repeat" $ do
        show (streamRepeat 1) `shouldBe` "[1,1,1,1,1,1,1,1,1,1]"

      it "stream map" $ do
        let xs = take 5 . streamToList . streamMap (+ 1) $ streamRepeat 1
         in xs `shouldBe` [2, 2, 2, 2, 2]

      it "stream from seed" $ do
        let xs = take 5 . streamToList $ streamFromSeed (+ 1) 1
         in xs `shouldBe` [1, 2, 3, 4, 5]

    describe "exercise 5: ruler" $ do
      it "first 12" $ do
        let xs = take 16 . streamToList $ ruler
         in xs `shouldBe` first16Ruler

    describe "exercise 6: stream polynomials" $ do
      it "fibs3" $ do
        take 15 (streamToList fibs3) `shouldBe` first15Fibs

    describe "exercise 7: matrix fibonacci" $ do
      it "0th fib" $ do
        fib4 0 `shouldBe` 0

      it "1st fib" $ do
        fib4 1 `shouldBe` 1

      it "2nd fib" $ do
        fib4 2 `shouldBe` 1

      it "13th fib" $ do
        fib4 13 `shouldBe` 233

      it "14th fib" $ do
        fib4 14 `shouldBe` 377

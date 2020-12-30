{-# OPTIONS_GHC -Wno-type-defaults #-}

module Week07.JoinListSpec where

import Week07.JoinList
import Week07.Sized
import Test.Hspec

import Data.Monoid

sizedJoinList :: JoinList Size String
sizedJoinList = 
  Append (Size 5)
    (Append (Size 2)
      (Single (Size 1) "aaa")
      (Single (Size 1) "bbb"))
    (Append (Size 3)
      (Single (Size 1) "ccc")
      (Append (Size 2)
        (Single (Size 1) "ddd")
        (Single (Size 1) "eee")))

spec :: Spec
spec =
  describe "Join List" $ do
    describe "exercise 1: append" $ do
      it "+++" $ do
        let x = Single (Product 5) "aaa"
            y = Single (Product 3) "bbb"
            a = x +++ y
         in a `shouldBe` Append (Product 15) x y

    describe "exercise 2: indexJ" $ do
      it "indexJ case -1" $ do
        indexJ (-1) sizedJoinList `shouldBe` jlToList sizedJoinList !!? (-1)
      it "indexJ case 0" $ do
        indexJ 0 sizedJoinList `shouldBe` jlToList sizedJoinList !!? 0
      it "indexJ case 1" $ do
        indexJ 1 sizedJoinList `shouldBe` jlToList sizedJoinList !!? 1
      it "indexJ case 2" $ do
        indexJ 2 sizedJoinList `shouldBe` jlToList sizedJoinList !!? 2
      it "indexJ case 3" $ do
        indexJ 3 sizedJoinList `shouldBe` jlToList sizedJoinList !!? 3
      it "indexJ case 4" $ do
        indexJ 4 sizedJoinList `shouldBe` jlToList sizedJoinList !!? 4
      it "indexJ case 5" $ do
        indexJ 5 sizedJoinList `shouldBe` jlToList sizedJoinList !!? 5
      it "indexJ case 6" $ do
        indexJ 6 sizedJoinList `shouldBe` jlToList sizedJoinList !!? 6

    describe "exercise 2: dropJ" $ do
      it "dropJ case 0" $ do
        jlToList (dropJ 0 sizedJoinList) `shouldBe` jlToList sizedJoinList
      it "dropJ case 1" $ do
        jlToList (dropJ 1 sizedJoinList) `shouldBe` drop 1 (jlToList sizedJoinList)
      it "dropJ case 2" $ do
        jlToList (dropJ 2 sizedJoinList) `shouldBe` drop 2 (jlToList sizedJoinList)
      it "dropJ case 3" $ do
        jlToList (dropJ 3 sizedJoinList) `shouldBe` drop 3 (jlToList sizedJoinList)
      it "dropJ case 4" $ do
        jlToList (dropJ 4 sizedJoinList) `shouldBe` drop 4 (jlToList sizedJoinList)
      it "dropJ case 5" $ do
        jlToList (dropJ 5 sizedJoinList) `shouldBe` drop 5 (jlToList sizedJoinList)
      it "dropJ case 6" $ do
        jlToList (dropJ 6 sizedJoinList) `shouldBe` drop 6 (jlToList sizedJoinList)

    describe "exercise 2: takeJ" $ do
      it "takeJ case 0" $ do
        jlToList (takeJ 0 sizedJoinList) `shouldBe` []
      it "takeJ case 1" $ do
        jlToList (takeJ 1 sizedJoinList) `shouldBe` take 1 (jlToList sizedJoinList)
      it "takeJ case 2" $ do
        jlToList (takeJ 2 sizedJoinList) `shouldBe` take 2 (jlToList sizedJoinList)
      it "takeJ case 3" $ do
        jlToList (takeJ 3 sizedJoinList) `shouldBe` take 3 (jlToList sizedJoinList)
      it "takeJ case 4" $ do
        jlToList (takeJ 4 sizedJoinList) `shouldBe` take 4 (jlToList sizedJoinList)
      it "takeJ case 5" $ do
        jlToList (takeJ 5 sizedJoinList) `shouldBe` take 5 (jlToList sizedJoinList)
      it "takeJ case 6" $ do
        jlToList (takeJ 6 sizedJoinList) `shouldBe` take 6 (jlToList sizedJoinList)

{-# OPTIONS_GHC -Wno-type-defaults #-}

module Week11.LectureSpec where

import Control.Applicative (ZipList(..))
import Test.Hspec

import Week11.Lecture

spec :: Spec
spec =
  describe "Lecture 11" $ do
    describe "*>" $ do
      it "maybe" $ do
        Just 1 .*> Just 2 `shouldBe` Just 1 *> Just 2
        Nothing .*> Just 2 `shouldBe` Nothing *> Just 2

      it "[]" $ do
        [1, 2, 3] .*> [4, 5] `shouldBe` [1, 2, 3] *> [4, 5]

      it "ZipList" $ do
        ZipList [1, 2] .*> ZipList [3, 4] `shouldBe` ZipList [1, 2] *> ZipList [3, 4]

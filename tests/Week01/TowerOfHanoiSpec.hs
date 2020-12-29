module Week01.TowerOfHanoiSpec where

import Week01.TowerOfHanoi
import Test.Hspec

spec :: Spec
spec =
  describe "Tower of Hanoi" $ do
    describe "example" $ do
      it "2 discs" $ do
        hanoi 2 "a" "b" "c" `shouldBe` [("a","c"), ("a","b"), ("c","b")]

    describe "higher number length checks" $ do
      it "3 disc length check" $ do
        length (hanoi 3 "a" "b" "c") `shouldBe` 7

      it "4 disc length check" $ do
        length (hanoi 4 "a" "b" "c") `shouldBe` 15

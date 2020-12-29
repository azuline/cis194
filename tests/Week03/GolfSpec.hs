module Week03.GolfSpec where

import Week03.Golf
import Test.Hspec

spec :: Spec
spec =
  describe "Golf" $ do
    describe "skip" $ do
      it "ABCD" $ do
        skips "ABCD" `shouldBe` ["ABCD", "BD", "C", "D"]

      it "hello!" $ do
        skips "hello!" `shouldBe` ["hello!", "el!", "l!", "l", "o", "!"]

      it "[1]" $ do
        skips [1 :: Integer] `shouldBe` [[1]]

      it "[True, False" $ do
        skips [True, False] `shouldBe` [[True, False], [False]]

      it "[]" $ do
        skips ([] :: [Integer]) `shouldBe` []

    describe "local maxima" $ do
      it "[2,9,5,6,1]" $ do
        localMaxima [2,9,5,6,1] `shouldBe` [9,6]
      it "[2,3,4,1,5]" $ do
        localMaxima [2,3,4,1,5] `shouldBe` [4]
      it "[1,2,3,4,5]" $ do
        localMaxima [1,2,3,4,5] `shouldBe` []

    describe "histogram" $ do
      it "[3,5]" $ do
        histogram [3, 5] `shouldBe` "   * *    \n==========\n0123456789\n"

      it "[1,4,5,4,...]" $ do
        histogram [1,4,5,4,6,6,3,4,2,4,9] `shouldBe` "    *     \n    *     \n    * *   \n ******  *\n==========\n0123456789\n"

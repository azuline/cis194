module Week01.CardValidatorSpec where

import Week01.CardValidator
import Test.Hspec

spec :: Spec
spec =
  describe "Card Validator" $ do
    describe "exercise 1: to digits" $ do
      it "forward 1234 to [1,2,3,4]" $ do
        toDigits 1234 `shouldBe` [1, 2, 3, 4]

      it "reverse 1234 to [4,3,2,1]" $ do
        toDigitsRev 1234 `shouldBe` [4, 3, 2, 1]

      it "edge case: 0" $ do
        toDigits 0 `shouldBe` []

      it "edge case: -17" $ do
        toDigits (-17) `shouldBe` []

    describe "exercise 2: double every other" $ do
      it "[8,7,6,5]" $ do
        doubleEveryOther [8, 7, 6, 5] `shouldBe` [16, 7, 12, 5]

      it "[1,2,3]" $ do
        doubleEveryOther [1, 2, 3] `shouldBe` [1, 4, 3]

    describe "exercise 3: sum digits" $ do
      it "[16,7,12,5]" $ do
        sumDigits [16, 7, 12, 5] `shouldBe` 22

    describe "exercise 4: validate" $ do
      it "true" $ do
        validate 4012888888881881 `shouldBe` True

      it "false" $ do
        validate 4012888888881882 `shouldBe` False

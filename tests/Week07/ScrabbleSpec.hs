module Week07.ScrabbleSpec where

import Week07.JoinList
import Week07.Scrabble
import Test.Hspec

spec :: Spec
spec =
  describe "Scrabble" $ do
    describe "exercise 3: scrabble" $ do
      it "scoreLine" $ do
        let jl = scoreLine "yay " +++ scoreLine "haskell!"
         in jl `shouldBe` Append (Score 23)
                            (Single (Score 9) "yay ")
                            (Single (Score 14) "haskell!")

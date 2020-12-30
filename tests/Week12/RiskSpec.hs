module Week12.RiskSpec where

import Control.Monad.Random
import Test.Hspec

import Week12.Risk

spec :: Spec
spec =
  describe "Risk" $ do
    describe "exercise 2: battle" $ do
      it "full strength armies!" $ do
        field <- evalRandIO $ battle (Battlefield 10 10)
        attackers field + defenders field `shouldBe` 18

      it "few attackers!" $ do
        field <- evalRandIO $ battle (Battlefield 2 10)
        attackers field + defenders field `shouldBe` 11

      it "few defenders!" $ do
        field <- evalRandIO $ battle (Battlefield 10 1)
        attackers field + defenders field `shouldBe` 10

    describe "exercise 3: invade" $ do
      it "invasion!!!" $ do
        field <- evalRandIO $ invade (Battlefield 10 10)
        attackers field < 2 || defenders field == 0 `shouldBe` True

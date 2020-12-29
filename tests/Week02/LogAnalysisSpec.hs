module Week02.LogAnalysisSpec where

import Week02.LogAnalysis
import Test.Hspec

spec :: Spec
spec =
  describe "Log Analysis" $ do
    it "what went wrong" $ do
      output <- testWhatWentWrong "data/week02/sample.log"
      let expected = ["Way too many pickles", "Bad pickle-flange interaction detected", "Flange failed!"]
      output `shouldBe` expected

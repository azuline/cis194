module Week04.WholemealSpec where

import Week04.Wholemeal
import Test.Hspec

spec :: Spec
spec =
  describe "Wholemeal" $ do
    describe "exercise 1: wholemeal programming" $ do
      it "fun1" $ do
        let xs = [1, 2, 3, 5, 2, 4, 7, 1, 2, 3, 99, 102]
         in fun1 xs `shouldBe` fun1' xs

      it "fun1 empty list" $ do
         fun1 [] `shouldBe` fun1' []

      it "fun2" $ do
         fun2 812 `shouldBe` fun2' 812

    describe "exercise 2: folding with trees" $ do
      it "three nodes" $ do
        foldTree "ABC" `shouldBe` Node 1 (Node 0 Leaf 'B' Leaf) 'C' (Node 0 Leaf 'A' Leaf)

      it "ten nodes" $ do
        foldTree "ABCDEFGHIJ" `shouldBe`
          Node 3
            (Node 2
              (Node 1 (Node 0 Leaf 'D' Leaf) 'G' Leaf)
              'I'
              (Node 1 (Node 0 Leaf 'A' Leaf) 'E' Leaf))
            'J'
            (Node 2
              (Node 1 (Node 0 Leaf 'B' Leaf) 'F' Leaf)
              'H'
              (Node 0 Leaf 'C' Leaf))

    describe "exercise 3: more folds!" $ do
      it "xor 1" $ do
        xor [False, True, False] `shouldBe` True

      it "xor 2" $ do
        xor [False, True, False, False, True] `shouldBe` False

      it "map" $ do
        map' (* (2 :: Integer)) [1, 2, 3, 4, 5] `shouldBe` [2, 4, 6, 8, 10]

      it "map empty list" $ do
        map' (* (2 :: Integer)) [] `shouldBe` []

    describe "exercise 4: finding primes" $ do
      it "n = 10" $ do
        sieveSundaram 10 `shouldBe` [3, 5, 7, 11, 13, 17, 19]

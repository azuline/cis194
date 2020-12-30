{-# OPTIONS_GHC -Wno-type-defaults #-}

module Week08.PartySpec where

import Data.Tree
import Test.Hspec

import Week08.Party
import Week08.Employee

gl1, gl2 :: GuestList
gl1 = GL [Emp "A" 1, Emp "B" 2] 3
gl2 = GL [Emp "C" 5, Emp "D" 8] 13

testTree :: Tree Integer
testTree = Node 1 [Node 2 [], Node 3 []]

spec :: Spec
spec =
  describe "Party" $ do
    describe "exercise 1: guest lists" $ do
      it "glCos" $ do
        glCons (Emp "C" 5) gl1 `shouldBe` GL [Emp "C" 5, Emp "A" 1, Emp "B" 2] 8

      it "monoid instance" $ do
         gl1 <> gl2 `shouldBe` GL [Emp "A" 1, Emp "B" 2, Emp "C" 5, Emp "D" 8] 16

      it "moreFun" $ do
         moreFun gl1 gl2 `shouldBe` gl2

    describe "exercise 2: tree fold" $ do
      -- These examples taken from hackage.haskell.org's foldTree docs.
      it "sum fold" $ do
        treeFold (\x xs -> sum (x:xs)) testTree `shouldBe` 6

      it "max fold" $ do
        treeFold (\x xs -> maximum (x:xs)) testTree `shouldBe` 3

      it "num leaves fold" $ do
        treeFold (\_ xs -> if null xs then 1 else sum xs) testTree `shouldBe` 2

      it "depth tree fold" $ do
        treeFold (\_ xs -> if null xs then 0 else 1 + maximum xs) testTree `shouldBe` 1

    describe "exercise 3: next level" $ do
      it "testing it" $ do
        let xs   = [(GL [Emp "A" 1] 1, GL [Emp "B" 2] 2), (GL [Emp "C" 3] 3, GL [Emp "D" 1] 1)]
            boss = Emp "FINAL BOSS" 999
            out  = (GL [boss, Emp "B" 2, Emp "D" 1] 1002, GL [Emp "B" 2, Emp "C" 3] 5)
         in nextLevel boss xs `shouldBe` out

    describe "exercise 4: max fun" $ do
      it "testing it" $ do
        maxFun testCompany `shouldBe`
          GL [Emp "John" 1, Emp "Sue" 5, Emp "Fred" 3, Emp "Sarah" 17] 26

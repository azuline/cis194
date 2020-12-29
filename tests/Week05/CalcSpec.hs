module Week05.CalcSpec where

import Week05.Calc
import Week05.ExprT
import Week05.StackVM (stackVM, StackVal(IVal))
import Test.Hspec

spec :: Spec
spec =
  describe "Calc" $ do
    describe "exercise 1: evaluator" $ do
      it "eval" $ do
        eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) `shouldBe` 20

    describe "exercise 2: evaluate string" $ do
      it "(2+3)*4" $ do
        evalStr "(2+3)*4" `shouldBe` Just 20

      it "2+3*4" $ do
        evalStr "2+3*4" `shouldBe` Just 14

      it "2+3*" $ do
        evalStr "2+3*" `shouldBe` Nothing

    describe "exercise 3: expr type class" $ do
      it "conv" $ do
        (mul (add (lit 2) (lit 3)) (lit 4) :: ExprT) `shouldBe` Mul (Add (Lit 2) (Lit 3)) (Lit 4)

    describe "exercise 5: stack vm" $ do
      it "(2+3)*4" $ do
        stackVM <$> compile "(2+3)*4" `shouldBe` Just (Right (IVal 20))

      it "2+3*4" $ do
        stackVM <$> compile "2+3*4" `shouldBe` Just (Right (IVal 14))

    describe "exercise 6: intermediate values" $ do
      it "3+6" $ do
        withVars [("x", 6)] (add (lit 3) (var "x")) `shouldBe` Just 9

      it "nothing" $ do
        withVars [("x", 6)] (add (lit 3) (var "y")) `shouldBe` Nothing

      it "6*(3+6)" $ do
        withVars [("x", 6), ("y", 3)] (mul (var "x") (add (var "y") (var "x"))) `shouldBe` Just 54

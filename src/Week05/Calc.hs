{-# LANGUAGE FlexibleInstances #-}

module Week05.Calc where

import qualified Data.Map as M

import qualified Week05.ExprT as E
import           Week05.ExprT (ExprT)
import           Week05.Parser (parseExp)
import           Week05.StackVM (Program, StackExp(..))

eval :: ExprT -> Integer
eval (E.Lit x) = x
eval (E.Add le re) = eval le + eval re
eval (E.Mul le re) = eval le * eval re

evalStr :: String -> Maybe Integer
evalStr s = eval <$> parseExp E.Lit E.Add E.Mul s

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = E.Lit
  add = E.Add
  mul = E.Mul

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = MinMax $ max x y
  mul (MinMax x) (MinMax y) = MinMax $ min x y

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit = Mod7 . (`mod` 7)
  add (Mod7 x) (Mod7 y) = Mod7 $ (x + y) `mod` 7
  mul (Mod7 x) (Mod7 y) = Mod7 $ (x * y) `mod` 7

testExp :: Expr a => String -> Maybe a
testExp = parseExp lit add mul

-- Exercise 5

instance Expr Program where
  lit x = [PushI x]
  add x y = x ++ y ++ [Add]
  mul x y = x ++ y ++ [Mul]

compile :: String -> Maybe Program
compile = testExp

-- Exercise 6

class HasVars a where
  var :: String -> a

data VarExprT =
    VLit Integer
  | VAdd VarExprT VarExprT
  | VMul VarExprT VarExprT
  | VVar String
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = VLit
  add = VAdd
  mul = VMul
  
instance HasVars VarExprT where
  var = VVar

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit       = const . Just
  add x y m = (+) <$> x m <*> y m
  mul x y m = (*) <$> x m <*> y m

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp' = exp' $ M.fromList vs

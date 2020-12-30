module Week04.Wholemeal where
  
import Data.List ((\\))

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n    = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/= 1) . iterate modifyNum
  where
    modifyNum n = if even n then n `div` 2 else 3 * n + 1

data Tree a =
    Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf
  where
    insert :: a -> Tree a -> Tree a
    insert x Leaf = Node 0 Leaf x Leaf
    insert x (Node h tl val tr)
      | hl < hr   = Node h tl' val tr
      | hl > hr   = Node h tl  val tr'
      | otherwise = Node (max hl' hr + 1) tl' val tr
      where
        tl' = insert x tl
        tr' = insert x tr
        hl  = height tl
        hl' = height tl'
        hr  = height tr

    height :: Tree a -> Integer
    height Leaf           = -1
    height (Node x _ _ _) = x

xor :: [Bool] -> Bool
xor = foldr (/=) False

{-# ANN module "HLint: ignore Use map" #-}
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

-- myFoldl :: (a -> b -> a) -> a -> [b] -> a
-- myFoldl f base xs = error "Do this when I wake up..."

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = (+ 1) . (* 2) <$> [1..n] \\ numbersToRemove
  where numbersToRemove = [ i + j + 2 * i * j | i <- [1..n],
                                                j <- [1..n],
                                                i <= j,
                                                i + j + 2 * i * j <= n ]

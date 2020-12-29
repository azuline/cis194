{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Week06.Fibonacci where

fib :: Integer -> Integer
fib 0          = 0
fib 1          = 1
fib n | n >= 2 = fib (n - 1) + fib (n - 2)
fib _          = error "Negative fibonacci not allowed!"

fibs1 :: [Integer]
fibs1 = fib <$> [0..]

fibs2 :: [Integer]
fibs2 = fst <$> scanl (\(x', y) _ -> (y, x' + y)) (0, 1) [0..]

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show = show . take 10 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons h t) = h : streamToList t

streamRepeat :: a -> Stream a
streamRepeat x' = Cons x' (streamRepeat x')

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x' xs) = Cons (f x') (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f s = Cons s (streamFromSeed f (f s))

nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

ruler :: Stream Integer
ruler = streamInterleave (streamRepeat 0) (streamMap (+ 1) ruler)

streamInterleave :: Stream a -> Stream a -> Stream a
streamInterleave (Cons x' xs) ys = Cons x' (streamInterleave ys xs)

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger x'                = Cons x' (streamRepeat 0)
  negate                        = streamMap negate
  (Cons x' xs) +    (Cons y ys) = Cons (x' + y) (xs + ys)
  (Cons x' xs) * s2@(Cons y ys) =
    Cons (x' * y) (streamMap (* x') ys + (xs * s2))

instance Fractional (Stream Integer) where
  s1@(Cons x' xs) / s2@(Cons y ys) =
    Cons (x' `div` y) (streamMap (`div` y) (xs - (s1 / s2) * ys))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

-- Matrix x1 x2 y1 y2
--
--   x1 x2
--   y1 y2
data Matrix = Matrix Integer Integer Integer Integer

instance Num Matrix where
  (Matrix x1 x2 y1 y2) * (Matrix x1' x2' y1' y2') =
    Matrix (x1 * x1' + x2 * y1')
           (y1 * x1' + y2 * y1')
           (x1 * x2' + x2 * y2')
           (y1 * x2' + y2 * y2')

fibMatrix :: Matrix
fibMatrix = Matrix 1 1 1 0

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = fibN
  where
    Matrix _ fibN _ _ = fibMatrix ^ n

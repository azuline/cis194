module Week07.JoinList where

import Week07.Sized
import Week07.Scrabble

data JoinList m a =
    Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
x +++ y = Append (tag x <> tag y) x y

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _ )  = m
tag (Append m _ _) = m

(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:_)  !!? 0         = Just x
(_:xs) !!? i         = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i (Single _ x)
  | i == 0      = Just x
  | otherwise   = Nothing
indexJ i (Append t l r)
  | i >= sz   = Nothing
  | i >= lSz  = indexJ (i - lSz) r
  | otherwise = indexJ i l
  where sz = getSize . size $ t
        lSz = getSize . size . tag $ l

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n jl@(Single _ _)
  | n == 0    = jl
  | otherwise = Empty
dropJ n (Append t l r)
  | n >= sz   = Empty
  | n >= lSz  = dropJ (n - lSz) r
  | otherwise = dropJ n l +++ r
  where sz = getSize . size $ t
        lSz = getSize . size . tag $ l

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n jl@(Single _ _)
  | n == 0    = Empty
  | otherwise = jl
takeJ n jl@(Append t l r)
  | n >= sz   = jl
  | n >= lSz  = l +++ takeJ (n - lSz) r
  | otherwise = takeJ n l
  where sz = getSize . size $ t
        lSz = getSize . size . tag $ l

scoreLine :: String -> JoinList Score String
scoreLine str = Single (scoreString str) str

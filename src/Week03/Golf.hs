{-# LANGUAGE TupleSections #-}

{-
I thought about golfing this, and I tried to, but the code was just
so ugly!

To golf, shorten variable names and inline functions. Don't want to
leave my code like that though.
-}

module Week03.Golf where

import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)

type HistogramValues = Map Integer Integer  

-- | Map get `n`th over all the `n`s.
skips :: [a] -> [[a]]
skips xs = getNthOf xs <$> [1..length xs]

-- | Get `n`th - Combine the list with the element indices and
--   retain those congruent to `0 mod n`. Then map away the indices.
getNthOf :: [a] -> Int -> [a]
getNthOf xs n = snd <$> filter ((== 0) . (`rem` n) . fst) (zip [1..] xs)

-- | Explicit recursion over all windows of size 3, finding the
--   local maxima.
localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:t)
  | y > x && y > z = y : localMaxima (y:z:t)
  | otherwise      =     localMaxima (y:z:t)
localMaxima _      = []

-- | Generate a map of digits to number of occurrences. Then
--   generate our histogram top-down from the maximum number
--   of occurrences for any given digit.
histogram :: [Integer] -> String
histogram xs = unlines strings ++ "==========\n0123456789\n"
  where
    values  = Map.fromListWith (+) $ (,1) <$> xs
    numRows = maximum . Map.elems $ values
    strings = makeRow values <$> [numRows,numRows - 1..1]

-- | Make a row of the histogram. If a digit has equal or more
--   occurrences to the row index, render it as a star;
--   otherwise, render it as a space.
makeRow :: HistogramValues -> Integer -> String
makeRow values row =
  [ if Map.findWithDefault 0 i values >= row then '*' else ' ' | i <- [0..9] ]

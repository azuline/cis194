module Week07.Scrabble where

import qualified Data.Char as C

newtype Score = Score Int
                deriving (Show, Eq)

instance Semigroup Score where
  Score x <> Score y = Score (x + y)

instance Monoid Score where
  mempty = Score 0

score :: Char -> Score
score c
  | c' `elem` "aeilnorstu" = Score 1
  | c' `elem` "dg"         = Score 2
  | c' `elem` "bcmp"       = Score 3
  | c' `elem` "fhvwy"      = Score 4
  | c' `elem` "k"          = Score 5
  | c' `elem` "jx"         = Score 8
  | c' `elem` "qz"         = Score 10
  | otherwise              = Score 0
    where c' = C.toLower c
  
scoreString :: String -> Score
scoreString = foldr (\c s -> s <> score c) (Score 0)

getScore :: Score -> Int
getScore (Score x) = x

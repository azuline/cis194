{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Week07.ScrabbleBuffer where

import Week07.Buffer
import Week07.JoinList
import Week07.Scrabble
import Week07.Sized

instance Buffer (JoinList (Score, Size) String) where
  -- | Convert the JoinList into a string.
  toString = unlines . jlToList

  -- | Convert a string into a JoinList.
  fromString = foldr (\s jl -> jl +++ Single (genTag s) s) Empty . lines
    where
      genTag str = (scoreString str, Size 1)

  -- | Get a line from the JoinList by index.
  line = indexJ

  -- | Replace a line in the JoinList.
  replaceLine idx newStr jl =
    takeJ idx jl +++ fromString newStr +++ dropJ (idx + 1) jl

  -- | Self-explanatory! Just get the size of the JoinList.
  numLines = getSize . snd . tag

  -- | Get the scrabble score.
  value = getScore . fst . tag

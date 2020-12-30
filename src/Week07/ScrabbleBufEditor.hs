module Week07.ScrabbleBufEditor where

import Week07.Buffer
import Week07.ScrabbleBuffer()
import Week07.JoinList
import Week07.Sized
import Week07.Scrabble
import Week07.Editor

-- | runhaskell -isrc src/Week07/ScrabbleBufEditor.hs
main :: IO()
main = runEditor editor (fromString "Scrabble buffer ^.~" :: JoinList (Score, Size) String)

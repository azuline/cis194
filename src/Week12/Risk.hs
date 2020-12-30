{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Week12.Risk where

import           Control.Monad.Random
import           Data.Functor         ((<&>))
import qualified Data.List as L
-- import           Debug.Trace

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving (Show)

battle :: Battlefield -> Rand StdGen Battlefield
battle field = do
  let numAtks = min 3 $ attackers field - 1
      numDefs = min 2 $ defenders field

  atks <- replicateM numAtks die <&> (L.sort . map unDV)
  defs <- replicateM numDefs die <&> (L.sort . map unDV)

  let matchups = zip atks defs
      lostAtks = length . filter (uncurry (>)) $ matchups
      lostDefs = length matchups - lostAtks

  pure $ Battlefield (attackers field - lostAtks) (defenders field - lostDefs)

invade :: Battlefield -> Rand StdGen Battlefield
invade field
  | attackers field < 2  = pure field
  | defenders field == 0 = pure field
  | otherwise            = battle field >>= invade

successProb :: Battlefield -> Rand StdGen Double
successProb field = do
  fields <- replicateM 1000 $ invade field
  let atkWins = length . filter ((== 0) . defenders) $ fields

  pure $ fromIntegral atkWins / 1000

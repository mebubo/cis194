{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List

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

dice :: Int -> Rand StdGen [DieValue]
dice n = sequence $ replicate n die

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving Show

decideStep :: Battlefield -> (DieValue, DieValue) -> Battlefield
decideStep bf (DV att, DV def) =
  if att > def
  then bf { defenders = (defenders bf) - 1 }
  else bf { attackers = (attackers bf) - 1 }

battle :: Battlefield -> Rand StdGen Battlefield
battle bf =
  let nAttackers = minimum [3, attackers bf - 1]
      nDefenders = minimum [2, defenders bf]
  in
    do
      attackerDieValues <- dice nAttackers
      defenderDieValues <- dice nDefenders
      let pairs = take 2 $ zip (sort attackerDieValues) (sort defenderDieValues)
      return $ foldl decideStep bf pairs


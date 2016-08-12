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
      getPairs attDice defDice = take 2 $ zip (sort attDice) (sort defDice)
      computeBattlefield attDice defDice = foldl decideStep bf (getPairs attDice defDice)
  in
      computeBattlefield <$> dice nAttackers <*> dice nDefenders

invade :: Battlefield -> Rand StdGen Battlefield
invade bf@(Battlefield a d) =
  if a < 2 || d <=0
  then return bf
  else battle bf >>= invade

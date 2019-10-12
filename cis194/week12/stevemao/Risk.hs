{-# OPTIONS_GHC -Wall #-}
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

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving Show

dieMany :: Int -> Rand StdGen [DieValue]
dieMany n = fmap (reverse . sort) . replicateM n $ die

compareTwoLists :: [DieValue] -> [DieValue] -> [Bool]
compareTwoLists (a : as) (d : ds) = (a > d) : compareTwoLists as ds
compareTwoLists [] _ = []
compareTwoLists _ [] = []

count   :: Eq a => a -> [a] -> Int
count x =  length . filter (==x)

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield a d) = do 
  let maxA = min (a - 1) 3
  let maxD = min d 2

  attacks <- dieMany maxA
  defends <- dieMany maxD

  let results = compareTwoLists attacks defends

  return . Battlefield (a - count False results) $ d - count True results

invade :: Battlefield -> Rand StdGen Battlefield
invade b@(Battlefield _ 0) = pure b
invade b@(Battlefield a _)
  | a < 2 = pure b
  | otherwise = battle b >>= invade

-- TODO: make the simulator run in parallel
successProb :: Battlefield -> Rand StdGen Double
successProb b = calculateResult 0 0
  where calculateResult :: Double -> Double -> Rand StdGen Double
        calculateResult 1000 attackersWins = pure (attackersWins / 1000)
        calculateResult n attackersWins = invade b >>= f
          where f (Battlefield _ 0) = calculateResult (n + 1) (attackersWins + 1)
                f _ = calculateResult (n + 1) attackersWins

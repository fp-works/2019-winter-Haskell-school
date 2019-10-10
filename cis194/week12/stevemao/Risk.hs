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

threeDies :: Rand StdGen [DieValue]
threeDies =
  die >>= \d1 ->
  die >>= \d2 ->
  die >>= \d3 ->
  return . reverse . sort $ [d1,d2,d3]

twoDies :: Rand StdGen [DieValue]
twoDies =
  die >>= \d1 ->
  die >>= \d2 ->
  return . reverse . sort $ [d1,d2]
  
oneDie :: Rand StdGen [DieValue]
oneDie = 
  die >>= \d ->
  return [d]

dieMany :: Int -> Rand StdGen [DieValue]
dieMany 1 = oneDie
dieMany 2 = twoDies
dieMany _ = threeDies

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

  return . Battlefield (a - count True results) $ d - count False results

invade :: Battlefield -> Rand StdGen Battlefield
invade b@(Battlefield _ 0) = pure b
invade b@(Battlefield a _)
  | a < 2 = pure b
  | otherwise = battle b >>= invade

calculateResult :: Double -> Double -> Rand StdGen Double
calculateResult 1000 attackersWins = pure (attackersWins / 1000)
calculateResult n attackersWins = invade (Battlefield 6 6) >>= f
  where f (Battlefield _ 0) = calculateResult (n + 1) (attackersWins + 1)
        f _ = calculateResult (n + 1) attackersWins

successProb :: Battlefield -> Rand StdGen Double
successProb b = calculateResult 0 0

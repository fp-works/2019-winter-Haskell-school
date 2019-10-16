{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List
import Data.Universe.Helpers
import Data.Tree
import Data.Bifunctor
import Debug.Trace

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num, Enum)

-- first :: (a -> b) -> (a, c) -> (b, c)
-- first f (a, c) = (f a, c)

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

sortReverse :: Functor f => f [DieValue] -> f [DieValue]
sortReverse = fmap $ reverse . sort

dieMany :: Int -> Rand StdGen [DieValue]
dieMany n = sortReverse . replicateM n $ die

cartprod :: [[a]] -> [[a]];
cartprod [] = [[]]
cartprod (xs:xss) = [x:ys | x<- xs, ys <-yss]
                        where yss = cartprod xss

diePlanned :: Int -> [[DieValue]]
diePlanned n = sortReverse . cartprod . replicate n $ [1..6]

count   :: Eq a => a -> [a] -> Int
count x =  length . filter (==x)

countSnd :: Eq a => a -> [(b, a)] -> Int
countSnd x = count x . fmap snd

battle' :: Monad m => (Int -> m [DieValue]) -> Battlefield -> m Battlefield
battle' m (Battlefield a d) = do 
  let maxA = min (a - 1) 3
  let maxD = min d 2

  attacks <- m maxA
  defends <- m maxD

  let results = zipWith (>) attacks defends

  return . Battlefield (a - count False results) $ d - count True results

battle :: Battlefield -> Rand StdGen Battlefield
battle = battle' dieMany

battleAll :: Battlefield -> [(Double, Battlefield)]
battleAll (Battlefield _ 0) = []
battleAll (Battlefield a d)
  | a < 2 = []
  | otherwise = do
      let maxA = min (a - 1) 3
      let maxD = min d 2

      let attackss = diePlanned maxA -- [[1,1,1], [2,1,1]...]
      let defendss = diePlanned maxD -- [[1,1], [2,1]...]

      let allPossibilities = attackss +*+ defendss -- [([1,1,1], [1,1]), ([2,1,1], [1,1])...]
      let allResults = fmap (uncurry $ zipWith (>)) allPossibilities

      let allResultsWithPercent = foldr (\curr acc -> if countSnd curr acc == 0 then (1, curr) : acc else fmap (\(co, cu) -> if cu == curr then (co + 1, cu) else (co, cu)) acc) [] allResults
      let allResultsNum = fromIntegral $ length allResults
      
      let ret = fmap (bimap (/ allResultsNum) (\r -> Battlefield (a - count False r) $ d - count True r)) allResultsWithPercent
      trace (show ret) ret

invade :: Battlefield -> Rand StdGen Battlefield
invade b@(Battlefield _ 0) = pure b
invade b@(Battlefield a _)
  | a < 2 = pure b
  | otherwise = battle b >>= invade

successProb :: Battlefield -> Rand StdGen Double
successProb = fmap (\bs -> foldr f 0 bs / fromIntegral simCount) . replicateM simCount . invade
  where f :: Battlefield -> Double -> Double
        f (Battlefield _ 0) acc = acc + 1
        f _ acc = acc
        simCount = 1000 :: Int

buildTree :: Battlefield -> Tree (Double, Battlefield)
buildTree b = unfoldTree f (1, b)
  where f :: (Double, Battlefield) -> ((Double, Battlefield), [(Double, Battlefield)])
        f c@(_, ba) = (c, battleAll ba)

calculateProb :: Tree (Double, Battlefield) -> Double
calculateProb = foldTree f
  where f :: (Double, Battlefield) -> [Double] -> Double
        f (percent, Battlefield _ 0) [] = percent
        f _ [] = 0
        f (percent, _) forest = sum . fmap (* percent) $ forest 

exactSuccessProb :: Battlefield -> Double
exactSuccessProb = calculateProb . buildTree

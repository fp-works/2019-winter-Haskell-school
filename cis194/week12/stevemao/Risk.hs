{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List
import Data.Universe.Helpers
import Data.Tree

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num, Enum)

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
  
-- data SteveTuple a b = SteveTuple (a, b)
-- 
-- instance Functor (SteveTuple a) where
--   -- fmap f (SteveTuple (a, a')) = SteveTuple (f a, f a')
-- 
-- instance Applicative (SteveTuple a) where
--   -- SteveTuple (a, a') <*> SteveTuple (b, b') = SteveTuple (a b, a' b')
-- 
-- instance Monad (SteveTuple a) where
--   SteveTuple (a, a') >>= f = SteveTuple (a >>= f, a' >>= b')
-- 
-- bibind

-- bibind :: (a1, m a) -> ((a1, a) -> ([a], m a)) -> m a
-- bibind = undefined
-- 
-- foldBind :: Monad m => m a -> ([a], m a) -> Int -> ([a], m a)
-- foldBind _ (as, m) 1 = (as, m)
-- foldBind m' (as, m) n = bibind (as, m) newM
--   where newM = \(as, r) -> (as ++ [r], m >>= )

sortReverse :: Functor f => f [DieValue] -> f [DieValue]
sortReverse = fmap (reverse . sort)

-- TODO: replicateM is wrong. Use fold bind
dieMany :: Int -> Rand StdGen [DieValue]
dieMany n = sortReverse . replicateM n $ die

count   :: Eq a => a -> [a] -> Int
count x =  length . filter (==x)

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

invade :: Battlefield -> Rand StdGen Battlefield
invade b@(Battlefield _ 0) = pure b
invade b@(Battlefield a _)
  | a < 2 = pure b
  | otherwise = battle b >>= invade

successProb :: Battlefield -> Rand StdGen Double
successProb b = fmap (\bs -> (foldr f 0 bs) / fromIntegral simCount) . replicateM simCount . invade $ b
  where f :: Battlefield -> Double -> Double
        f (Battlefield _ 0) acc = acc + 1
        f _ acc = acc
        simCount = 1000 :: Int

cartprod :: [[a]] -> [[a]];
cartprod [] = [[]]
cartprod (xs:xss) = [x:ys | x<- xs, ys <-yss]
                        where yss = cartprod xss

diePlanned :: Int -> [[DieValue]]
diePlanned n = sortReverse . cartprod . replicate n $ [1..6]

battleAll :: Battlefield -> [Battlefield]
battleAll (Battlefield a d) = do
  let maxA = min (a - 1) 3
  let maxD = min d 2

  let attackss = diePlanned maxA -- [[1,1,1], [2,1,1]...]
  let defendss = diePlanned maxD -- [[1,1], [2,1]...]

  let allResults = if length (concat attackss) > 0 && length (concat defendss) > 0 then attackss +*+ defendss else [] -- [([1,1,1], [1,1]), ([2,1,1], [1,1])...]

  fmap (\r -> Battlefield (a - count False r) $ d - count True r) $ fmap (\result -> uncurry (zipWith (>)) result) allResults

buildTree :: Battlefield -> Tree Battlefield
buildTree = unfoldTree f
  where f :: Battlefield -> (Battlefield, [Battlefield])
        f b = (b, battleAll b)

calculateProb :: Tree Battlefield -> Double
calculateProb = foldTree f
  where f :: Battlefield -> [Double] -> Double
        f (Battlefield _ 0) [] = 1
        f _ [] = 0
        f _ forest = sum forest / fromIntegral (length forest)

exactSuccessProb :: Battlefield -> Double
exactSuccessProb = calculateProb . buildTree

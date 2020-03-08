{-# OPTIONS_GHC -fno-warn-orphans #-}
module Party where

import           Data.List (sort)
import           Data.Tree (Tree (Node, rootLabel, subForest))
import           Employee

-- ex1
glCons :: Employee -> GuestList -> GuestList
glCons e gl = GL (e:el) (f + glf)
  where
    Emp { empFun = f } = e
    GL el glf = gl


instance Semigroup GuestList where
  GL l1 f1 <> GL l2 f2 = GL (l1 ++ l2) (f1 + f2)

instance Monoid GuestList where
  mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ f1) gl2@(GL _ f2)
  | f1 < f2   = gl2
  | otherwise = gl1


-- ex2
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f Node { rootLabel = l, subForest = sf } = f l (map (treeFold f) sf)
-- treeFold f (Node l sf) = f l (map (treeFold f) sf)


-- ex3
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss l = (wb, wob)
  where
    wb  = glCons boss mempty <> mconcat (map snd l)
    wob = mconcat (map fst l)


-- ex4
maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel


-- ex5
getFun :: GuestList -> Fun
getFun (GL _ f) = f

getSortedNames :: GuestList -> [Name]
getSortedNames (GL l _) = sort . map (\(Emp n _) -> n) $ l

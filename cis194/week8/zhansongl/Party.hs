{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Employee
import Data.Tree (Tree, rootLabel, subForest)
import Data.Foldable (fold)

glCons :: Employee -> GuestList -> GuestList
glCons e (GL l f) = GL (e:l) (f + empFun e)

instance Semigroup GuestList where
  (<>) (GL l1 f1) (GL l2 f2) = GL (l1 ++ l2) (f1 + f2)

instance Monoid GuestList where
  mempty = GL [] 0

getFun :: GuestList -> Fun
getFun (GL _ f) = f

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f tree = f root . fmap (treeFold f) $ forest
  where root = rootLabel tree
        forest = subForest tree

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss prev = (withBoss, withoutBoss)
  where withBoss = glCons boss . fold . fmap snd $ prev
        withoutBoss = fold . fmap (uncurry moreFun) $ prev

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

getInvitees :: GuestList -> [Employee]
getInvitees (GL xs _) = xs

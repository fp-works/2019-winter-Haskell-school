{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Employee
import Data.Tree (Tree, rootLabel, subForest)

glCons :: Employee -> GuestList -> GuestList
glCons e (GL l f) = GL (e:l) (f + empFun e)

instance Semigroup GuestList where
  (<>) (GL l1 f1) (GL l2 f2) = GL (l1 ++ l2) (f1 + f2)

instance Monoid GuestList where
  mempty = GL [] 0

getFun :: GuestList -> Fun
getFun (GL _ f) = f

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1 gl2 = case compare (getFun gl1) (getFun gl2) of
                    GT -> gl1
                    _  -> gl2

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f tree = f root . map (treeFold f) $ forest
  where root = rootLabel tree
        forest = subForest tree

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss prev = (withBoss, withoutBoss)
  where withBoss = glCons boss . foldMap id . map snd $ prev
        withoutBoss = foldMap id . map (uncurry moreFun) $ prev

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

getInvitees :: GuestList -> [Employee]
getInvitees (GL xs _) = xs

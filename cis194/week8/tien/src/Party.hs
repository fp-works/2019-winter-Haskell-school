{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Data.Foldable
import Data.List
import Data.Tree
import Employee

-- exercie 1 --
-- 1.1 --
glCons :: Employee -> GuestList -> GuestList
glCons em@(Emp _ emf) (GL empL fun) = GL (em : empL) (fun + emf)

-- 1.2 --
instance Semigroup GuestList where
  (GL empL1 fun1) <> (GL empL2 fun2) = GL (empL1 ++ empL2) (fun1 + fun2)

instance Monoid GuestList where
  mempty = GL [] 0

-- 1.3 --
moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- exercie 2 --
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node l []) = f l []
treeFold f (Node l s) = f l $ treeFold f <$> s

-- exercie 3 --
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel emp gls = (gl1, gl2)
  where
    gl1 = glCons emp . fold . fmap snd $ gls
    gl2 = fold $ moreFun <$> fst <*> snd <$> gls

-- exercie 4 --
maxFun :: Tree Employee -> GuestList
maxFun = moreFun <$> fst <*> snd <$> treeFold nextLevel

-- exercie 5 --
totalFun :: Tree Employee -> Fun
totalFun = getFun . maxFun

totalEmpNameList :: Tree Employee -> [String]
totalEmpNameList = sort . fmap getEmpName . getEmpList . maxFun

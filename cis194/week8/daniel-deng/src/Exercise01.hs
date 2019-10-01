{-# OPTIONS_GHC -fno-warn-orphans #-}

module CIS194.Homework08.Exercise01 (glCons, moreFun) where

import CIS194.Homework08.Employee (Employee(..), GuestList(..))

instance Semigroup GuestList where
  (GL gs1 f1) <> (GL gs2 f2) = GL (gs1 ++ gs2) (f1 + f2)

instance Monoid GuestList where
  mempty = GL [] 0

{-
Adds an Employee to the GuestList (updating the cached Fun score appropriately)
Of course, in general this is impossible:
the updated fun score should depend on whether the Employee
being added is already in the list, or if any of their direct subordinates are in the list, and so on. For our purposes, though, you
may assume that none of these special cases will hold: that is,
glCons should simply add the new Employee and add their fun
score without doing any kind of checks.
-}
glCons :: Employee -> GuestList -> GuestList
glCons e (GL gs f) = GL (e : gs) (f + empFun e)

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

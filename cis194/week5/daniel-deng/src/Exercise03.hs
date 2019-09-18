module CIS194.Homework05.Exercise03 (Expr(..), reify) where

import CIS194.Homework05.ExprT

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

reify :: ExprT -> ExprT
reify = id

module Expr (Expr(..)) where

class Expr a where
  add :: a -> a -> a
  mul :: a -> a -> a
  lit :: Integer -> a

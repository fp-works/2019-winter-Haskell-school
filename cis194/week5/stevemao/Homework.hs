{-# OPTIONS_GHC -Wall #-}
module Homework where
import ExprT
import Parser

eval :: ExprT -> Integer
eval (Lit a) = a
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a
  
instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

instance Expr MinMax where
  lit = MinMax
  add (MinMax a) (MinMax b) = lit (max a b)
  mul (MinMax a) (MinMax b) = lit (min a b)

instance Expr Mod7 where
  lit = Mod7 . (`mod` 7)
  add (Mod7 a) (Mod7 b) = lit (a + b)
  mul (Mod7 a) (Mod7 b) = lit (a * b)

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

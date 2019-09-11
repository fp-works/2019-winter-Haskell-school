module Exercise1 (eval) where

import           ExprT

eval :: ExprT -> Integer
eval (Lit n)       = n
eval (Add lhs rhs) = eval lhs + eval rhs
eval (Mul lhs rhs) = eval lhs * eval rhs

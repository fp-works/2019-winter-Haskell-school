module CIS194.Homework05.Exercise01 (eval) where

import CIS194.Homework05.ExprT

eval :: ExprT -> Integer
eval (Lit x)   = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

{-# OPTIONS_GHC -Wall #-}
module Calc where
import ExprT

eval :: ExprT -> Integer
eval (Lit a) = a
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

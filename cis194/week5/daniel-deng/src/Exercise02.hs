module CIS194.Homework05.Exercise02 (evalStr) where

import CIS194.Homework05.ExprT
import CIS194.Homework05.Parser
import CIS194.Homework05.Exercise01

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul

module Exercise2 (evalStr) where

import           Control.Applicative (liftA)

import           ExprT
import           Parser

import           Exercise1

evalStr :: String -> Maybe Integer
evalStr = liftA eval . parseExp Lit Add Mul

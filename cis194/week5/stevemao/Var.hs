{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
module Var where
import qualified Data.Map as M
import Homework

class HasVars a where
  var :: String -> a

data VarExprT = Lit Integer
           | Add VarExprT VarExprT
           | Mul VarExprT VarExprT
           | Var String
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = Lit
  add = Add
  mul = Mul

instance HasVars VarExprT where
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit a = (\_ -> Just a)
  add a b = (\m -> fmap (+) (a m) <*> (b m))
  mul a b = (\m -> fmap (*) (a m) <*> (b m))

withVars :: [(String, Integer)]
  -> (M.Map String Integer -> Maybe Integer)
  -> Maybe Integer
withVars vs exp = exp $ M.fromList vs

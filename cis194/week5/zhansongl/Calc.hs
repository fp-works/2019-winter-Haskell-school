{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Calc where

import ExprT
import Parser
import Control.Applicative (liftA)
import qualified StackVM as S
import qualified Data.Map as M

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

evalStr :: String -> Maybe Integer
evalStr = liftA eval . parseExp Lit Add Mul

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

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (>0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7   = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = MinMax $ max x y
  mul (MinMax x) (MinMax y) = MinMax $ min x y

instance Expr Mod7 where
  lit = Mod7 . (`mod` 7)
  add (Mod7 x) (Mod7 y) = Mod7 $ (x + y) `mod` 7
  mul (Mod7 x) (Mod7 y) = Mod7 $ (x * y) `mod` 7

testExp :: Expr a => String -> Maybe a
testExp = parseExp lit add mul

instance Expr S.Program where
  lit n = [S.PushI n]
  add p1 p2 = p1 ++ p2 ++ [S.Add]
  mul p1 p2 = p1 ++ p2 ++ [S.Mul]

compile :: String -> Maybe S.Program
compile = testExp

class HasVars a where
  var :: String -> a

data VarExprT = VarLit Integer
              | Var String
              | VarAdd VarExprT VarExprT
              | VarMul VarExprT VarExprT
  deriving (Show, Eq)

instance HasVars VarExprT where
  var = Var

instance Expr VarExprT where
  lit = VarLit
  add = VarAdd
  mul = VarMul

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit = const . Just
  add e1 e2 m = (+) <$> (e1 m) <*> (e2 m)
  mul e1 e2 m = (*) <$> (e1 m) <*> (e2 m)

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs expr = expr . M.fromList $ vs


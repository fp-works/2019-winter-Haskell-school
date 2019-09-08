{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Exercises where

import           Control.Applicative (liftA2)
import qualified Data.Map            as M
import qualified ExprT               as E
import           Parser              (parseExp)
import qualified StackVM             as S

-- ex1
eval :: E.ExprT -> Integer
eval (E.Lit x)     = x
eval (E.Add e1 e2) = eval e1 + eval e2
eval (E.Mul e1 e2) = eval e1 * eval e2


-- ex2
evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp E.Lit E.Add E.Mul


-- ex3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr E.ExprT where
  lit = E.Lit
  add = E.Add
  mul = E.Mul

reify :: E.ExprT -> E.ExprT
reify = id


-- ex4
-- Bool
instance Expr Bool where
  lit = (>0)
  add = (||)
  mul = (&&)

-- MinMax
newtype MinMax = MinMax { minMaxValue :: Integer } deriving (Eq, Show)
instance Expr MinMax where
  lit                       = MinMax
  add (MinMax a) (MinMax b) = MinMax $ max a b
  mul (MinMax a) (MinMax b) = MinMax $ min a b

-- Mod7
newtype Mod7 = Mod7 { mod7Value :: Integer } deriving (Eq, Show)
instance Expr Mod7 where
  lit a                 = Mod7 (a `mod` 7)
  add (Mod7 a) (Mod7 b) = lit $ a + b
  mul (Mod7 a) (Mod7 b) = lit $ a * b


-- ex5
instance Expr S.Program where
  lit a = [S.PushI a]
  add a b = a ++ b ++ [S.Add]
  mul a b = a ++ b ++ [S.Mul]

compile :: String -> Maybe S.Program
compile = parseExp lit add mul


-- ex6
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
  lit = const . Just
  -- map (+) over Maybe
  -- add f1 f2 m = liftA2 (+) (f1 m) (f2 m)
  -- map (liftA2 (+)) over reader applicative
  -- add f1 f2 = liftA2 (+) <$> f1 <*> f2
  -- add f1 f2 = liftA2 (liftA2 (+)) f1 f2
  add = liftA2 . liftA2 $ (+)
  mul = liftA2 . liftA2 $ (*)

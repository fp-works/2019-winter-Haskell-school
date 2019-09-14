{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}
module Calc
  ( eval
  , evalStr
  , Expr(lit, add, mul)
  , MinMax(MinMax)
  , Mod7(Mod7)
  , compile
  , HasVars(var)
  )
where

import qualified Data.Map                      as M
import qualified ExprT
import           Parser
import qualified StackVM                       as VM
import           Control.Monad                  ( liftM2 )

-- Exercise 1
eval :: ExprT.ExprT -> Integer
eval (ExprT.Lit x  ) = x
eval (ExprT.Add x y) = eval x + eval y
eval (ExprT.Mul x y) = eval x * eval y

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr (parseExp ExprT.Lit ExprT.Add ExprT.Mul -> Just expr) = Just $ eval expr
evalStr _ = Nothing

-- Exercise 3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT.ExprT where
  lit = ExprT.Lit
  add = ExprT.Add
  mul = ExprT.Mul

-- Exercise 4
instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit x | x > 0     = True
        | otherwise = False
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = MinMax $ max x y
  mul (MinMax x) (MinMax y) = MinMax $ min x y

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
  lit x = Mod7 $ x `mod` 7
  add (Mod7 x) (Mod7 y) = Mod7 $ (x + y) `mod` 7
  mul (Mod7 x) (Mod7 y) = Mod7 $ (x * y) `mod` 7

-- Exercise 5
instance Expr VM.Program where
  lit x = [VM.PushI x]
  add x y = x ++ y ++ [VM.Add]
  mul x y = x ++ y ++ [VM.Mul]

compile :: String -> Maybe VM.Program
compile = parseExp lit add mul

-- Exercise 6

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
  add x y m = liftM2 (+) (x m) (y m)
  mul x y m = liftM2 (*) (x m) (y m)

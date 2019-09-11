{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Calc where

import qualified ExprT   as ET
import           Parser
import           StackVM as SVM

-- Exercise 1 --
eval :: ET.ExprT -> Integer
eval (ET.Lit a)   = a
eval (ET.Add a b) = eval a + eval b
eval (ET.Mul a b) = eval a * eval b

-- Exercise 2 --
evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp ET.Lit ET.Add ET.Mul

-- Exercise 3 --
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ET.ExprT where
  lit = ET.Lit
  add = ET.Add
  mul = ET.Mul

reify :: ET.ExprT -> ET.ExprT
reify = id

-- Exercise 4 --
newtype MinMax =
  MinMax Integer
  deriving (Eq, Show)

newtype Mod7 =
  Mod7 Integer
  deriving (Eq, Show)

instance Expr Integer where
  lit a = a
  add a b = a + b
  mul a b = a * b

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

instance Expr MinMax where
  lit a = MinMax a
  add (MinMax a) (MinMax b) = MinMax . max a $ b
  mul (MinMax a) (MinMax b) = MinMax . min a $ b

instance Expr Mod7 where
  lit a = Mod7 $ mod a 7
  add (Mod7 a) (Mod7 b) = Mod7 . lit $ (a + b)
  mul (Mod7 a) (Mod7 b) = Mod7 . lit $ (a * b)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger :: Maybe Integer
testInteger = testExp

testBool :: Maybe Bool
testBool = testExp

testMM :: Maybe MinMax
testMM = testExp

testSat :: Maybe Mod7
testSat = testExp

-- Exercise 5 --
instance Expr SVM.Program where
  lit a = [SVM.PushI a]
  add a b = a ++ b ++ [SVM.Add]
  mul a b = a ++ b ++ [SVM.Mul]

compile :: String -> Maybe SVM.Program
compile = parseExp lit add mul

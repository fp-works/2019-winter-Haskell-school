{-# OPTIONS_GHC -fno-warn-orphans #-}

module CIS194.Homework05.Exercise04 (MinMax(..), Mod7(..)) where

import CIS194.Homework05.Exercise03 (Expr(..))

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (>0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax { minMaxValue :: Integer } deriving (Eq, Show)
instance Expr MinMax where
  lit = MinMax
  add (MinMax a) (MinMax b) = MinMax $ max a b
  mul (MinMax a) (MinMax b) = MinMax $ min a b

newtype Mod7 = Mod7 { mod7Value :: Integer } deriving (Eq, Show)
instance Expr Mod7 where
  lit a                 = Mod7 (a `mod` 7)
  add (Mod7 a) (Mod7 b) = lit $ a + b
  mul (Mod7 a) (Mod7 b) = lit $ a * b

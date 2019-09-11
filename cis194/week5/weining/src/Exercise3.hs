module Exercise3
  ( Expr(..)
  , retify
  ) where

import           Expr
import           ExprT

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

retify :: ExprT -> ExprT
retify = id

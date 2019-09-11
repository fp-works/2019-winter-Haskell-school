module Exercise4
  ( testExp
  , testInteger
  , testBool
  , testMM
  , MinMax(..)
  , testSat
  , Mod7(..)
  ) where

import           Data.Bool

import           Expr
import           Parser

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
  lit = MinMax
  add (MinMax l) (MinMax r) = MinMax $ max l r
  mul (MinMax l) (MinMax r) = MinMax $ min l r

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
  lit = Mod7 . (`mod` 7)
  add (Mod7 l) (Mod7 r) = lit (l + r)
  mul (Mod7 l) (Mod7 r) = lit (l * r)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger  = testExp :: Maybe Integer
testBool     = testExp :: Maybe Bool
testMM       = testExp :: Maybe MinMax
testSat      = testExp :: Maybe Mod7

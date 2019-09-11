{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Exercise5 (compile) where

import           Expr
import           Parser
import           StackVM

compile :: String -> Maybe Program
compile s = parseExp lit add mul s

instance Expr Program where
  lit n = [PushI n]
  add lhs rhs = lhs ++ rhs ++ [Add]
  mul lhs rhs = lhs ++ rhs ++ [Mul]


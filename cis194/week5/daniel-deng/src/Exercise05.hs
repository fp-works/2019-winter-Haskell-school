{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module CIS194.Homework05.Exercise05 where

import CIS194.Homework05.Exercise03
import CIS194.Homework05.Parser
import CIS194.Homework05.StackVM

instance Expr Program where
  lit a = [PushI a]
  add a b = a ++ b ++ [Add]
  mul a b = a ++ b ++ [Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
module Compile where
import Homework
import StackVM
import Parser

instance Expr Program where
  lit a = [PushI a]
  add a b = a ++ b ++ [Add]
  mul a b = a ++ b ++ [Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul

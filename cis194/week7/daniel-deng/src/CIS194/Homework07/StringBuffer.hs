{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module CIS194.Homework07.StringBuffer where

import Data.Monoid ()

import CIS194.Homework07.Buffer

instance Buffer String where
  toString     = id
  fromString   = id
  line n b     = safeIndex n (lines b)
  replaceLine n l = unlines . uncurry replaceLine' . splitAt n . lines
      where replaceLine' pre [] = pre
            replaceLine' pre (_:ls) = pre ++ l:ls
  numLines     = length . lines
  value        = length . words

safeIndex :: Int -> [a] -> Maybe a
safeIndex n _ | n < 0 = Nothing
safeIndex _ []        = Nothing
safeIndex 0 (x:_)     = Just x
safeIndex n (_:xs)    = safeIndex (n-1) xs

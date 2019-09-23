{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module StringBuffer where

import Data.Monoid ()

import Buffer

-- to avoid orphan instance warning
newtype BufferString = BufferString { getString :: String }

instance Buffer BufferString where
  toString     = getString . id
  fromString   = id . BufferString
  line n       = safeIndex n . lines . toString
  replaceLine n l = fromString . unlines . uncurry replaceLine' . splitAt n . lines . toString
      where replaceLine' pre [] = pre
            replaceLine' pre (_:ls) = pre ++ l:ls
  numLines     = length . lines . toString
  value        = length . words . toString

safeIndex :: Int -> [a] -> Maybe a
safeIndex n _ | n < 0 = Nothing
safeIndex _ []        = Nothing
safeIndex 0 (x:_)     = Just x
safeIndex n (_:xs)    = safeIndex (n-1) xs
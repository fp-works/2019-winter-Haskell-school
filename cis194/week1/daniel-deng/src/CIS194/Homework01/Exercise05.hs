module CIS194.Homework01.Exercise05 (hanoi) where

type Disks = Integer
type Peg   = String
type Move  = (Peg, Peg)

{-
  To move n discs (stacked in increasing size) from peg a to peg b
  using peg c as temporary storage,
  1. move n − 1 discs from a to c using b as temporary storage
  2. move the top disc from a to b
  3. move n − 1 discs from c to b using a as temporary storage.
-}
hanoi :: Disks -> Peg -> Peg -> Peg -> [Move]
hanoi n start destination temp
  | 0 >= n    = []
  | otherwise = hanoi n' start temp destination
                ++ [(start, destination)]
                ++ hanoi n' temp destination start
              where
                n' = n - 1

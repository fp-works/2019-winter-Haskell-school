module Exercise6 ( hanoi4 ) where

import           Data.List
import           Data.Ord

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 from to _    = [(from, to)]
hanoi n from to temp =
     hanoi (n - 1) from temp to
  ++ hanoi 1 from to temp
  ++ hanoi (n - 1) temp to from

shortest :: [[a]] -> [a]
shortest [] = []
shortest xs = minimumBy (comparing length) xs

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 from to temp1 temp2 = []
hanoi4 1 from to temp1 temp2 = [(from, to)]
hanoi4 n from to temp1 temp2 =
  shortest [  hanoi4 (n - k) from temp2 temp1 to
           ++ hanoi k from to temp1
           ++ hanoi4 (n - k) temp2 to from temp1
           | k <- [1..(n-1)] ]

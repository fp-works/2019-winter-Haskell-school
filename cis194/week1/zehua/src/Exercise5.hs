module Exercise5 where

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b _ = [(a, b)]
hanoi n a b c =
    hanoi n1 a c b ++ [(a, b)] ++ hanoi n1 c b a
  where
    n1 = n - 1

module Exercise4 (sieveSundaram) where

{-
Start with a list of the integers from 1 to n.
From this list, remove all numbers of the form i + j + 2ij where:
    i , j ∈ N ,   1 ≤ i ≤ j
    i + j + 2 i j ≤ n
-}
numsRemoved :: Integer -> [Integer]
numsRemoved n = [ i + j + 2 * i * j | i <- [1..n], j <- [i..n] ]

{-
The remaining numbers are doubled and incremented by one,
giving a list of the odd prime numbers
(i.e., all primes except 2) below 2n + 2.
-}
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = 2 : [ 2 * x + 1 | x <- [1..n], not (x `elem` (numsRemoved n)) ]


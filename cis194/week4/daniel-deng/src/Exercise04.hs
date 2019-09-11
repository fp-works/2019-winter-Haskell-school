module CIS194.Homework04.Exercise04 (sieveSundaram) where

cartProd :: [a] -> [(a, a)]
cartProd xs = [(a, b) | a <- xs, b <- xs]

validNumber :: Integer -> Bool
validNumber x = not
              . any (\(i, j) -> i + j + 2 * i * j == x)
              . filter (uncurry (<=))
              . cartProd
              $ [1..x]

sieveSundaram :: Integer -> [Integer]
sieveSundaram = map ((+1) . (*2))
              . filter validNumber
              . enumFromTo 1

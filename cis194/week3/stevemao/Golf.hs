module Golf where

getSecondIfMax :: (Integer, Integer, Integer) -> [Integer] -> [Integer]
getSecondIfMax (a, b, c) list
  | b > a && b > c = [b] ++ list
  | otherwise = list

localMaxima :: [Integer] -> [Integer]
localMaxima list = foldr getSecondIfMax [] $ zip3 list (drop 1 list) (drop 2 list)

exercise2 = do
  print $ localMaxima [2,9,5,6,1]
  print $ localMaxima [2,3,4,1,5]
  print $ localMaxima [1,2,3,4,5]

  print $ localMaxima [2,9,5,6,1] == [9,6]
  print $ localMaxima [2,3,4,1,5] == [4]
  print $ localMaxima [1,2,3,4,5] == []

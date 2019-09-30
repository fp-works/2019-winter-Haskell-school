module Golf where
import Data.List.HT

skips :: [a] -> [[a]]
skips list = fmap (\(item, index) -> tail . sieve index $ (head list : list)) . zip list $ [1..]

getSecondIfMax :: (Integer, Integer, Integer) -> [Integer] -> [Integer]
getSecondIfMax (a, b, c) list
  | b > a && b > c = [b] ++ list
  | otherwise = list

localMaxima :: [Integer] -> [Integer]
localMaxima list = foldr getSecondIfMax [] $ zip3 list (drop 1 list) (drop 2 list)

exercise1 = do
  print $ skips "ABCD" == ["ABCD", "BD", "C", "D"]
  print $ skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
  print $ skips [1] == [[1]]
  print $ skips [True,False] == [[True,False], [False]]
  print $ skips ([] :: [Int]) == []

exercise2 = do
  print $ localMaxima [2,9,5,6,1] == [9,6]
  print $ localMaxima [2,3,4,1,5] == [4]
  print $ localMaxima [1,2,3,4,5] == []

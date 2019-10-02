module Main where
import Golf

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

exercise3 = do
  print $ histogram [1,1,1,5] == " *        \n *        \n *   *    \n==========\n0123456789\n"
  print $ histogram [1,4,5,4,6,6,3,4,2,4,9] == "    *     \n    *     \n    * *   \n ******  *\n==========\n0123456789\n"
  print $ histogram [3,5] == "   * *    \n==========\n0123456789\n"

main :: IO ()
main = do
  exercise1
  exercise2
  exercise3

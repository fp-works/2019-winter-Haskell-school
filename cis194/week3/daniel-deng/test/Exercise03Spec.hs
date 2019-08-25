module CIS194.Homework03.Exercise03Spec where

import CIS194.Homework03.Exercise03

import Text.Printf

import Test.Tasty.Hspec

t :: [Integer] -> String -> Spec
t input expectedOutput =
  it (printf "test input: %s" (show input)) $
    histogram input `shouldBe` expectedOutput

spec_histogram :: Spec
spec_histogram = do

  t [1,1,1,5] " *        \n *        \n *   *    \n==========\n0123456789\n"
  t [1,4,5,4,6,6,3,4,2,4,9] "    *     \n    *     \n    * *   \n ******  *\n==========\n0123456789\n"

{-
Prelude> putStr " *        \n *        \n *   *    \n==========\n0123456789\n"
 *
 *
 *   *
==========
0123456789
Prelude> putStr "    *     \n    *     \n    * *   \n ******  *\n==========\n0123456789\n"
    *
    *
    * *
 ******  *
==========
0123456789
-}

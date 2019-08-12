-- test/LibSpec.hs
module Main where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Lib

main :: IO ()
main = do
  defaultMain
    (testGroup
       "Exercise tests"
       [ toDigitsTestWith123
       , toDigitsTestWith0
       , toDigitsTestWithNegative
       , toDigitsRevTestWith123
       , toDigitsRevTestWith0
       , toDigitsRevTestWithNegative
       , buildWeightsTestWith3
       , buildWeightsTestWith6
       , doubleEveryOtherTestWith153
       , doubleEveryOtherTestWith8735
       , sumDigitsWith4Digits
       , sumDigitsWithEmpty
       , validateTestWithValidCcNum
       , validateTestWithInvalidCcNum
       , hanoi3PegTest
       ])

---- Exercise one ----
toDigitsTestWith123 :: TestTree
toDigitsTestWith123 =
  testCase
    "Test toDigits with 123"
    (assertEqual "should show [1,2,3]" [1, 2, 3] (toDigits 123))

toDigitsTestWith0 :: TestTree
toDigitsTestWith0 =
  testCase "Test toDigits with 0" (assertEqual "should show []" [] (toDigits 0))

toDigitsTestWithNegative :: TestTree
toDigitsTestWithNegative =
  testCase
    "Test toDigits with negatives"
    (assertEqual "should show []" [] (toDigits (-12)))

toDigitsRevTestWith123 :: TestTree
toDigitsRevTestWith123 =
  testCase
    "Test toDigitsRev with 123"
    (assertEqual "should show [3,2,1]" [3, 2, 1] (toDigitsRev 123))

toDigitsRevTestWith0 :: TestTree
toDigitsRevTestWith0 =
  testCase
    "Test toDigitsRev with 0"
    (assertEqual "should show []" [] (toDigitsRev 0))

toDigitsRevTestWithNegative :: TestTree
toDigitsRevTestWithNegative =
  testCase
    "Test toDigitsRev with negatives"
    (assertEqual "should show []" [] (toDigitsRev (-12)))

---- Exercise two ----
buildWeightsTestWith3 :: TestTree
buildWeightsTestWith3 =
  testCase
    "Test buildWeights with 3"
    (assertEqual "should show [1,2,1]" [1, 2, 1] (buildWeights 3))

buildWeightsTestWith6 :: TestTree
buildWeightsTestWith6 =
  testCase
    "Test buildWeights with 6"
    (assertEqual "should show [1,2,1,2,1,2]" [1, 2, 1, 2, 1, 2] (buildWeights 6))

doubleEveryOtherTestWith153 :: TestTree
doubleEveryOtherTestWith153 =
  testCase
    "Test doubleEveryOther with 153"
    (assertEqual "should show [1,10,3]" [1, 10, 3] (doubleEveryOther [1, 5, 3]))

doubleEveryOtherTestWith8735 :: TestTree
doubleEveryOtherTestWith8735 =
  testCase
    "Test doubleEveryOther with 8735"
    (assertEqual
       "should show [16,7,6,5]"
       [16, 7, 6, 5]
       (doubleEveryOther [8, 7, 3, 5]))

---- Exercise three ----
sumDigitsWith4Digits :: TestTree
sumDigitsWith4Digits =
  testCase
    "Test sumDigits with 16,7,12,5"
    (assertEqual "should show 22" 22 (sumDigits [16, 7, 12, 5]))

sumDigitsWithEmpty :: TestTree
sumDigitsWithEmpty =
  testCase
    "Test sumDigits with empty array"
    (assertEqual "should show 0" 0 (sumDigits []))

---- Exercise four ----
validateTestWithValidCcNum :: TestTree
validateTestWithValidCcNum =
  testCase
    "Test validate with a valid credit card number"
    (assertEqual "should return True" True (validate 4012888888881881))

validateTestWithInvalidCcNum :: TestTree
validateTestWithInvalidCcNum =
  testCase
    "Test validate with a valid credit card number"
    (assertEqual "should return False" False (validate 4012888888881882))

---- Exercise five ----
hanoi3PegTest :: TestTree
hanoi3PegTest =
  testCase
    "Test hanoi 3 pegs abc with 2"
    (assertEqual
       "should returne [(\"a\",\"c\"),(\"a\",\"b\"),(\"b\",\"c\")]"
       [("a", "c"), ("a", "b"), ("c", "b")]
       (hanoi 2 "a" "b" "c"))

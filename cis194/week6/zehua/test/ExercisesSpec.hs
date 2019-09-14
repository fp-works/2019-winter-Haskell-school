module ExercisesSpec where

import           Exercises
import           Safe       (atMay)
import           Test.Hspec


spec :: Spec
spec = do
  describe "ex1" $ do
    it "works" $ do
      take 10 fibs1 `shouldBe` [0,1,1,2,3,5,8,13,21,34]

  describe "ex2" $ do
    it "works" $ do
      take 20 fibs2 `shouldBe` [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181]

  describe "ex4" $ do
    it "works for repeat" $ do
      take 3 (streamToList (streamRepeat (2 :: Integer))) `shouldBe` [2,2,2]
      show (streamRepeat (3 :: Integer)) `shouldBe` "[3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3]"

    it "works for map" $ do
      take 3 (streamToList (streamMap (+3) (streamRepeat (2 :: Integer)))) `shouldBe` [5,5,5]

    it "works for from seed" $ do
      take 5 (streamToList (streamFromSeed (+1) (1 :: Integer))) `shouldBe` [1,2,3,4,5]

    it "is equivalent to iterate after streamToList" $ do
      take 10 (streamToList (streamFromSeed (*2) (2 :: Integer))) `shouldBe` take 10 (iterate (*2) (2 :: Integer))
      take 20 (streamToList (streamFromSeed (\a -> a*a) (3 :: Integer))) `shouldBe` take 20 (iterate (\a -> a*a) (3 :: Integer))

  describe "ex5" $ do
    it "works for nats" $ do
      take 101 (streamToList nats) `shouldBe` [1..101]

    it "works for ruler" $ do
      take 16 (streamToList ruler) `shouldBe` [0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4]

  describe "ex6" $ do
    it "works for Num" $ do
      take 101 (streamToList nats) `shouldBe` [1..101]

    it "works for fibs3" $ do
      take 19 fibs3 `shouldBe` [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584]

  describe "ex7" $ do
    it "works for Matrix multiplication" $ do
      Matrix 1 1 1 0 * f1 `shouldBe` Matrix 2 1 1 1
      Matrix 3 2 2 1 * f1 `shouldBe` Matrix 5 3 3 2

    it "works for fib4" $ do
      map fib4 [0..19] `shouldBe` [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181]
      atMay fibs2 100000 `shouldBe` Just (fib4 100000)


import Fibonacci

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "fibs1" $ do
    it "the first few values should be correct" $ do
      take 13 fibs1 `shouldBe` [0,1,1,2,3,5,8,13,21,34,55,89,144]

  describe "fibs2" $ do
    it "should have [0,1] as the first two elements" $ do
      take 2 fibs2 `shouldBe` [0,1]
    it "should generate correct fibanocci sequence" $ property $
      forAll (arbitrary :: Gen (Small Int))
        (\x -> let n = getSmall x
                in isFibonacciSequence $ take n fibs2)

  describe "nats" $ do
    it "should return i at index i" $ do
      forAll (choose (0, 10000))
        (\n -> (streamToList nats !! fromIntegral n) == n)

  describe "ruler" $ do
    it "should the correct result at index i" $ do
      forAll (choose (0, 1000))
        (\n -> let r = (streamToList ruler !! n)
                in ((n+1) `rem` (2^r)) == 0 && ((n+1) `rem` (2^(r+1))) /= 0)

  describe "fibs3" $ do
    it "should return the correct fibonacci sequence" $ do
      (take 10 . streamToList $ fibs3) `shouldBe` (take 10 fibs2)

  describe "fib4" $ do
    it "should return the correct fibonacci number" $ property $
      forAll (choose (0, 10000))
        (\n -> (fib4 . fromIntegral $ n) == (fibs2 !! n))

isFibonacciSequence :: [Integer] -> Bool
isFibonacciSequence [] = True
isFibonacciSequence [x] = True
isFibonacciSequence [x,y] = True
isFibonacciSequence (x:y:z:zs) = z == (x+y) && isFibonacciSequence (y:z:zs)


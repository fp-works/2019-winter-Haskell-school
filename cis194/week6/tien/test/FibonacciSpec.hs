import Fibonacci
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "Fibonacci" $ do
      describe "exercise 1:" $ do
        it "should return correct Fibonacci by given an Integer" $ do
          fib 0 `shouldBe` 0
          fib 1 `shouldBe` 1
          fib 6 `shouldBe` 8
        it "should generate an infinite Fibonacci sequence" $ do
          take 10 fibs1 `shouldBe` [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
      describe "exercise 2:" $ do
        it "should generate the same as fibs1 for first n elements with O(n)" $ do
          take 10 fibs2 `shouldBe` take 10 fibs1
      describe "exercise 3 & 4:" $ do
        it "should show 20 elements of a stream composed of 5" $ do
          show (streamRepeat 5) `shouldBe`
            "[5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5]"
        it "should show 20 elements of a stream composed of 6" $ do
          show (streamMap (+ 1) . streamRepeat $ 5) `shouldBe`
            "[6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6]"
        it
          "should show 20 elements of a stream generated from seed 1 and unfold (*2)" $ do
          show (streamFromSeed (* 2) 1) `shouldBe`
            "[1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384,32768,65536,131072,262144,524288]"
        it "should show 20 elements of a ruler stream" $ do
          show ruler `shouldBe` "[0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,0,1,0,2]"

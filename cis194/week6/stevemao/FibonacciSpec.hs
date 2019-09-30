{-# LANGUAGE BlockArguments #-}
module FibonacciSpec where
import Fibonacci
import Test.Hspec
import Protolude

at19 = 4181
firstFewSamples = [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,at19]

samples = length firstFewSamples

takeSamples = take samples
takeToList = takeSamples . streamToList
repeatTake = takeSamples . repeat

spec :: Spec
spec = do
  it "fib" do
    fib (fromIntegral (length firstFewSamples) - 1) `shouldBe` at19

  it "fibs1" do
    takeSamples fibs1 `shouldBe` firstFewSamples

  it "fibs2" do
    takeSamples fibs2 `shouldBe` firstFewSamples

  it "streamRepeat" do
    (takeToList . streamRepeat $ "val") `shouldBe` repeatTake "val"

  it "streamMap" do
    (takeToList . streamMap (+1) . streamRepeat $ samples) `shouldBe` repeatTake 21

  it "streamFromSeed" do
    (takeToList . streamFromSeed (+1) $ samples) `shouldBe` (takeSamples . iterate (+1) $ samples)
     
  it "nats" do
    takeToList nats `shouldBe` (takeSamples . iterate (+1) $ 0)
    
  it "interleaveStreams" do
    takeToList (interleaveStreams (streamRepeat 0) (streamRepeat 1)) `shouldBe` takeSamples [0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1]

  it "ruler" do
    takeToList ruler `shouldBe` takeSamples [0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4,0,1,0,2]

  it "fibs3" do
    takeToList fibs3 `shouldBe` firstFewSamples

  it "fib4" do
    fib4 (fromIntegral (length firstFewSamples) - 1) `shouldBe` at19

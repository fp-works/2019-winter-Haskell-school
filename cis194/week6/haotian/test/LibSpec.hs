module LibSpec where
import Lib 
import Test.Hspec

spec :: Spec
spec = do
  describe "Exercises 1" $ do
    it "return correct data" $ do
      take 10 fibs1 `shouldBe` [0,1,1,2,3,5,8,13,21,34]
  describe "Exercises 2" $ do
    it "return correct data" $ do
      take 10 fibs2 `shouldBe` [0,1,1,2,3,5,8,13,21,34]
  describe "Exercises 3" $ do
    it "should return correct result" $ do
      show (streamRepeat 1) `shouldBe`
            "[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]"
    it "should return correct result" $ do
      show (streamMap (*2) . streamRepeat $ 1) `shouldBe`
            "[2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2]"
    it "should return correct result" $ do
      show (streamFromSeed (*2) 1) `shouldBe`
            "[1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384,32768,65536,131072,262144,524288]"
  describe "Exercises 5" $ do
    it "nats" $ do
      take 16 (streamToList nats) `shouldBe` [0..15]

    it "ruler" $ do
      take 16 (streamToList ruler) `shouldBe` [0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4]

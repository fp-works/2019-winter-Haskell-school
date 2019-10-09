import CreditCards

import Test.QuickCheck
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "toDigits" $ do
    it "should produce a reverse list of all digits of a positive Integer" $ property $
      forAll (arbitrary :: Gen (Positive Integer))
        (\n' -> let joinDigits = foldr (++) "" . map show
                    n = getPositive n'
                 in (reverse . joinDigits $ toDigits n) == (show n))
    it "should return empty list for invalid values" $ do
      toDigits (-1) `shouldBe` []
      toDigits 0 `shouldBe` []

  describe "doubleEveryOther" $ do
    it "should produce list with double sum for padding: [1,2,3] => [0,1,0,2,0,3]" $ property $
      forAll (arbitrary :: Gen [Integer])
        (\xs -> let padList [] = []
                    padList [x] = [0,x]
                    padList (x : xs) = (0 : x : padList xs)
                 in (2 * sum xs) == sum (doubleEveryOther . padList $ xs))
    it "should produce list with the same sum for padding: [1,2,3] => [1,0,2,0,3,0]" $ property $
      forAll (arbitrary :: Gen [Integer])
        (\xs -> let padList [] = []
                    padList [x] = [x,0]
                    padList (x : xs) = (x : 0 : padList xs)
                 in (sum xs) == sum (doubleEveryOther . padList $ xs))

  describe "sumDigits" $ do
    it "should return sum of all digits" $ do
      (sumDigits [12, 34, 56]) `shouldBe` (1 + 2 + 3 + 4 + 5 + 6)
      (sumDigits [0, 34, 56]) `shouldBe` (3 + 4 + 5 + 6)

  describe "validate" $ do
    it "should return True for valid numbers" $ do
      (validate 4012888888881881) `shouldBe` True
    it "should return False for invalid numbers" $ do
      (validate 4012888888881882) `shouldBe` False

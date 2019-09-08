import CreditCards

import Test.QuickCheck
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Credit card number verification" $ do
    describe "toDigits quickchecks" $ do
      it "toDigitsRev" $ property $
        \x -> toDigitsRev_prop x
      it "toDigits" $ property $
        \x -> toDigits_prop x

    describe "doubleEveryOther quickchecks" $ do
      describe "doubleEveryOtherRev with different paddings" $ do
        it "[1,2,3] => [0,1,0,2,0,3]" $ property $
          \xs -> doubleEveryOtherRev_prop1 xs
        it "[1,2,3] => [1,0,2,0,3,0]" $ property $
          \xs -> doubleEveryOtherRev_prop2 xs
        it "[1,2,3] => [1,0,2,0,3]" $ property $
          \xs -> doubleEveryOtherRev_prop3 xs
      describe "doubleEveryOther with different paddings" $ do
        it "[1,2,3] => [0,1,0,2,0,3]" $ property $
          \xs -> doubleEveryOther_prop1 xs
        it "[1,2,3] => [1,0,2,0,3,0]" $ property $
          \xs -> doubleEveryOther_prop2 xs
        it "[1,2,3] => [1,0,2,0,3]" $ property $
          \xs -> doubleEveryOther_prop3 xs

    describe "sumDigits quickchecks" $ do
      it "sumDigits_prop" $ property $ sumDigits_prop

    describe "validate works" $ do
      it "should return True for valid numbers" $ do
        (validate 4012888888881881) `shouldBe` True
      it "should return False for invalid numbers" $ do
        (validate 4012888888881882) `shouldBe` False

toDigitsRev_prop n = (reverse . joinDigits $ toDigitsRev n) == (showDigits n)
  where types = n :: Integer
        joinDigits xs = foldr (++) "" $ map show xs
        showDigits n
          | n <= 0 = "" -- weird restriction, but okay
          | otherwise = show n

toDigits_prop n = (joinDigits $ toDigits n) == (showDigits n)
  where types = n :: Integer
        joinDigits xs = foldr (++) "" $ map show xs
        showDigits n
          | n <= 0 = "" -- weird restriction, but okay
          | otherwise = show n

doubleEveryOtherRev_prop1 xs = (2 * (sum xs)) == sum (doubleEveryOtherRev $ padList xs)
  where types = xs :: [Integer]
        padList [] = []
        padList [x] = [0,x]
        padList (x : xs) = (0 : x :padList xs)

doubleEveryOtherRev_prop2 xs = (sum xs) == sum (doubleEveryOtherRev $ padList xs)
  where types = xs :: [Integer]
        padList [] = []
        padList [x] = [x,0]
        padList (x : xs) = (x : 0 : padList xs)

doubleEveryOtherRev_prop3 xs = (sum xs) == sum (doubleEveryOtherRev $ padList xs)
  where types = xs :: [Integer]
        padList [] = []
        padList [x] = [x]
        padList (x : xs) = (x : 0 : padList xs)

-- [1,2,3] ==> [0,1,0,2,0,3]
doubleEveryOther_prop1 xs = (sum xs) == sum (doubleEveryOther $ padList xs)
  where types = xs :: [Integer]
        padList [] = []
        padList [x] = [0,x]
        padList (x : xs) = (0 : x : padList xs)

-- [1,2,3] ==> [1,0,2,0,3,0]
doubleEveryOther_prop2 xs = (2 * (sum xs)) == sum (doubleEveryOther $ padList xs)
  where types = xs :: [Integer]
        padList [] = []
        padList (x : xs) = (x : 0 : padList xs)

-- [1,2,3] ==> [1,0,2,0,3]
doubleEveryOther_prop3 xs = (sum xs) == sum (doubleEveryOther $ padList xs)
  where types = xs :: [Integer]
        padList [] = []
        padList [x] = [x]
        padList (x : xs) = (x : 0 : padList xs)

genNonNegativeLists :: Gen [Integer]
genNonNegativeLists = suchThat arbitrary nonNegative
  where nonNegative [] = True
        nonNegative (x : xs) = (x >= 0) && (nonNegative xs)

sumDigits_prop =
  forAll genNonNegativeLists $
    \xs -> (sumDigits xs) == (sum . numDigits $ strDigits xs)
  where strDigits xs = foldr (++) "" $ map show xs
        numDigits s
          | s == "" = [0]
          | otherwise = toDigits $ read s


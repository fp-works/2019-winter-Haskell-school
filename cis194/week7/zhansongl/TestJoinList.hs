import JoinList
import Sized
import Scrabble
import Buffer

import Control.Applicative (liftA2)
import Data.Monoid

-- need safe
import Safe

import Test.Hspec
import Test.QuickCheck

instance (Monoid m, Arbitrary a, Arbitrary m) => Arbitrary (JoinList m a) where
  arbitrary = sized arbitraryJoinList

arbitraryJoinList :: (Monoid m, Arbitrary m, Arbitrary a) => Int -> Gen (JoinList m a)
arbitraryJoinList 0 = pure Empty
arbitraryJoinList n = do
    m <- arbitrary
    a <- arbitrary
    oneof [pure $ Single m a, liftA2 (+++) sublist sublist]
  where sublist = arbitraryJoinList (n `div` 2)

validTags :: (Eq m, Monoid m) => (JoinList m a) -> Bool
validTags Empty = True
validTags (Single _ _) = True
validTags (Append m l1 l2) = m == (tag l1 <> tag l2)

validSize :: (Sized m) => (JoinList m a) -> Bool
validSize Empty = True
validSize (Single s _) = size s == Size 1
validSize (Append s l1 l2) =
  size s == size l1 + size l2 && validSize l1 && validSize l2

replaceTagWithSize :: JoinList m a -> JoinList Size a
replaceTagWithSize Empty = Empty
replaceTagWithSize (Single _ a) = Single (Size 1) a
replaceTagWithSize (Append _ l1 l2) = l1' +++ l2'
  where l1' = replaceTagWithSize l1
        l2' = replaceTagWithSize l2

main :: IO ()
main = hspec $ do
  describe "+++" $ do
    it "should produce JoinList that has valid tags" $ property $
      forAll (arbitrary :: Gen (JoinList (Sum Int) Char)) validTags

  describe "indexJ" $ do
    it "should produce data at the correct index" $ property $
      forAll (arbitrary :: Gen (JoinList (Sum Int) Char, Small Int))
        (\(l, i) -> let l' = replaceTagWithSize l
                        i' = getSmall i
                     in (indexJ i' l') == (jlToList l' `atMay` i'))

  describe "dropJ" $ do
    it "should drop exactly n number of elements at the beginning" $ property $
      forAll (arbitrary :: Gen (JoinList (Sum Int) Char, Positive (Small Int)))
        (\(l, n) -> let n' = getSmall . getPositive $ n
                        l' = dropJ n' . replaceTagWithSize $ l
                     in jlToList l' == drop n' (jlToList l) && validSize l')
    it "can drop everything in the left sublist correctly" $ do
      (jlToList . dropJ 1 $
                    (Append (Size 2) (Single (Size 1) 'a') (Single 1 'b')))
      `shouldBe` "b"

  describe "takeJ" $ do
    it "should take exactly n number of elements at the beginning" $ property $
      forAll (arbitrary :: Gen (JoinList (Sum Int) Char, Positive (Small Int)))
        (\(l, n) -> let n' = getSmall . getPositive $ n
                        l' = takeJ n' . replaceTagWithSize $ l
                     in jlToList l' == take n' (jlToList l) && validSize l')
    it "can take everything in the left sublist correctly" $ do
      (jlToList . takeJ 1 $
                    (Append (Size 2) (Single (Size 1) 'a') (Single 1 'b')))
      `shouldBe` "a"

  describe "scoreLine" $ do
    it "should return correct scrabble scores" $ do
      scoreLine "yay " `shouldBe` (Single (Score 9) "yay ")
      scoreLine "haskell!" `shouldBe` (Single (Score 14) "haskell!")

  describe "Score" $ do
    it "should yield correct scrabble score when joined with (+++)" $ do
      scoreLine "yay " +++ scoreLine "haskell!"
      `shouldBe` Append (Score 23)
                        (Single (Score 9) "yay ")
                        (Single (Score 14) "haskell!")

  describe "Buffer JoinList" $ do
    it "should satisfy: toString . fromString == id" $ do
      forAll (arbitrary :: Gen [String])
        (\xs -> let s = unlines xs
                    jl = (fromString s) :: JoinList (Score, Size) String
                 in toString jl == s)
    it "should replace line given valid index" $ do
      forAll (arbitrary :: Gen ([String], NonNegative Int))
        (\(xs,n) -> let s = unlines xs
                        jl = (fromString s) :: JoinList (Score, Size) String
                        n' = getNonNegative n
                        jl' = replaceLine n' "YAY" jl
                     in size jl == size jl'
                     && (line n' jl') == case compare (Size n') (size jl') of
                                           LT -> Just "YAY"
                                           _  -> Nothing)


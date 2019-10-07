import Homework
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "fun1'" $ do
    it "should behave just like fun1" $ property $
      \xs -> (fun1' xs) == (fun1 xs)

  describe "fun2'" $ do
    it "should behave just like fun2" $ property $
      forAll (arbitrary :: Gen (Positive Integer))
        (\x -> (fun2' $ getPositive x) == (fun2 $ getPositive x))

  describe "foldTree" $ do
    it "should produce balanced trees" $ property $
      forAll (arbitrary :: Gen String)
        (\s -> let t = foldTree s
               in  balanced t && correctHeight t)

  describe "xor" $ do
    it "should return False for empty list" $ do
      xor [] `shouldBe` False
    it "should return correct value" $ do
      xor [False, True, False] `shouldBe` True
      xor [False, True, False, False, True] `shouldBe` False

  describe "map'" $ do
    it "should behave exactly like map" $ property $
      forAll (arbitrary :: Gen [Integer])
        (\xs -> map' (+2) xs `shouldBe` map (+2) xs)

  describe "myFoldl" $ do
    it "should behave exactly like foldl" $ property $
      forAll (arbitrary :: Gen [String])
        (\xs -> myFoldl (++) "__" xs `shouldBe` foldl (++) "__" xs)

  describe "sieveSundaram" $ do
    it "should return the correct prime numbers" $ do
      sieveSundaram 10 `shouldBe` [3, 5, 7, 11, 13, 17, 19]
      sieveSundaram 40 `shouldBe` [3, 5, 7, 11, 13, 17, 19,
                                   23, 29, 31, 37, 41, 43,
                                   47, 53, 59, 61, 67, 71,
                                   73, 79]

balanced :: Tree a -> Bool
balanced Leaf = True
balanced (Node h left _ right) =
  (abs $ (getHeight left) - (getHeight right)) <= 1
  && balanced left
  && balanced right

calcHeight :: Tree a -> Integer
calcHeight Leaf = (-1)
calcHeight (Node _ left _ right) =
  1 + max (calcHeight left) (calcHeight right)

correctHeight :: Tree a -> Bool
correctHeight Leaf = True
correctHeight t@(Node h left _ right) =
  (calcHeight t) == h
  && correctHeight left
  && correctHeight right


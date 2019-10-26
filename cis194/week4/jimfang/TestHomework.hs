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

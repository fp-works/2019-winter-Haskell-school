module ExercisesSpec where

-- import           Control.Applicative (liftA2)
import           Control.Monad (liftM2)
import           Exercises
import           Test.Hspec

spec :: Spec
spec = do
  describe "ex1.fun1" $ do
    it "works with empty" $ do
      fun1' [] `shouldBe` fun1 []

    it "works with non-empty" $ do
      mapM_ (liftM2 shouldBe fun1' fun1)
        [ [1,3,4]
        , [1]
        , [2]
        , [1,2,3]
        , [1,3..11]
        , [2,4..20]
        , [3..100] ]

  describe "ex1.fun2" $ do
    it "works with 1" $ do
      fun2' 1 `shouldBe` fun2 1

    it "works with positive integers" $ do
      mapM_ (liftM2 shouldBe fun2' fun2)
        [2..10]


module ExercisesSpec where

import           Exercises
import           ExprT
import           Test.Hspec


spec :: Spec
spec = do
  describe "ex1" $ do
    it "works" $ do
      mapM_ (\(e, v) -> eval e `shouldBe` v)
        [ (Lit 3, 3)
        , (Add (Lit 2) (Lit 3), 5)
        , (Mul (Lit 2) (Lit 3), 6)
        , (Mul (Add (Lit 2) (Lit 3)) (Lit 4), 20)
        , (Add (Mul (Lit 2) (Lit 3)) (Mul (Lit 4) (Add (Lit 5) (Lit 6))), 50) ]


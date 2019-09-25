import Data.Monoid
import Test.Hspec

import JoinList

main :: IO ()
main =
  hspec $ do
    describe "JoinList" $ do
      describe "exercise 1" $ do
        it "should return correct JoinList" $ do
          jl1 +++ jl2 `shouldBe` (Append (Product 150) jl1 jl2)
  where
    jl1 = Single (Product 5) 'e'
    jl2 = Append (Product 30) (Single (Product 10) 'g') (Single (Product 3) 'h')

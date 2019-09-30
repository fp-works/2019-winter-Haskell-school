module CIS194.Homework07.Exercise01Spec where

import CIS194.Homework07.JoinList ( JoinList(..) )
import CIS194.Homework07.Exercise01

import Data.Monoid

import Test.Tasty.Hspec

jl1 :: JoinList (Product Int) Char
jl1 = Single (Product 3) 'a'

jl2 :: JoinList (Product Int) Char
jl2 = Single (Product 6) 'b'

jl3 :: JoinList (Product Int) Char
jl3 = Append (Product 14) (Single (Product 2) 'x') (Single (Product 7) 'y')

jl12 :: JoinList (Product Int) Char
jl12 = Append (Product 18) jl1 jl2

jl13 :: JoinList (Product Int) Char
jl13 = Append (Product 42) jl1 jl3

jl123 :: JoinList (Product Int) Char
jl123 = Append (Product (18 * 42)) jl12 jl13

spec_JoinListAppend :: Spec
spec_JoinListAppend = do

  it "test left identity" $
    Empty +++  jl1 `shouldBe ` jl1

  it "test right identity" $
    jl1 +++ Empty `shouldBe` jl1

  it "Single +++ Single" $
    jl1 +++ jl2 `shouldBe` jl12

  it "Single +++ Append" $
    jl1 +++ jl3 `shouldBe` jl13

  it "Append +++ Append" $
    jl12 +++ jl13 `shouldBe` jl123

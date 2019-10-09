module PartySpec where

import           Data.Tree
import           Employee
import           Party                   hiding ( main )

import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "glCons"
    $          it "should add employee and funscore"
    $          glCons (Emp "John" 3) (GL [(Emp "Jack" 2)] 2)
    `shouldBe` (GL [(Emp "John" 3), (Emp "Jack" 2)] 5)

  describe "treeFold"
    $ it "should fold all values of a tree"
    $ treeFold (\x -> (+ x) . sum) (Node 1 [Node 2 [Node 4 []], Node 3 []])
    `shouldBe` 10

  describe "maxFun"
    $          it "should get max fun for test company"
    $          (getFun . maxFun $ testCompany)
    `shouldBe` 26

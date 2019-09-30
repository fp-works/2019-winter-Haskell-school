import Party

import Test.Hspec
import Employee
import Data.Tree (Tree(Node), rootLabel, subForest)

treeFoldTest =
  describe "treeFold" $ do
    it "should be able to sum all values in a tree" $ do
      (treeFold (\r xs -> r + sum xs) t) `shouldBe` 10
        where t = Node { rootLabel = 1,
                         subForest = [Node { rootLabel = 2,
                                             subForest = [Node { rootLabel = 3,
                                                                 subForest = [] }]
                                           },
                                      Node { rootLabel = 4, subForest = [] }] }

nextLevelTest =
  describe "nextLevel" $ do
    it "should generate the correct next level" $ do
      (nextLevel boss prev) `shouldBe` (GL [boss] 1000000, GL [employee1, employee2] 17)
        where boss = Emp { empName = "Garfield", empFun = 1000000 }
              employee1 = Emp { empName = "Jon", empFun = 5 }
              employee2 = Emp { empName = "Odie", empFun = 12 }
              prev = [(GL [employee1] 5, mempty), (GL [employee2] 12, mempty)]

maxFunTest =
  describe "maxFun" $ do
    it "should return the guest list that generates the maximum fun" $ do
      (getFun $ maxFun testCompany) `shouldBe` 26
      (getFun $ maxFun testCompany2) `shouldBe` 26

main :: IO ()
main = hspec $ do
  treeFoldTest
  nextLevelTest
  maxFunTest

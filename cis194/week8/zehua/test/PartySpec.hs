module PartySpec where

import           Data.Tree  (Tree ( Node ), rootLabel)
import           Employee
import           Party
import           Test.Hspec


testBoss :: Employee
testBoss = rootLabel testCompany

testEmp1 :: Employee
testEmp1 = Emp "employee 1" 5

testEmp2 :: Employee
testEmp2 = Emp "employee 2" 7

testEmp3 :: Employee
testEmp3 = Emp "employee 3" 9

testEmp4 :: Employee
testEmp4 = Emp "employee 4" 4

-- build a singleton GuestList with one employee
sGL :: Employee -> GuestList
sGL e = glCons e mempty

testGL1n2 :: GuestList
testGL1n2 = sGL testEmp1 <> sGL testEmp2

countEmployees :: Tree Employee -> Integer
countEmployees = treeFold (\_ l -> 1 + sum l)

sumEmployeeFun :: Tree Employee -> Fun
sumEmployeeFun = treeFold (\(Emp _ f) l -> f + sum l)

employeeNames :: Tree Employee -> [Name]
employeeNames = treeFold (\(Emp n _) l -> n : concat l)

spec :: Spec
spec = do
  describe "ex1" $ do
    it "works with glCons" $ do
      glCons testBoss testGL1n2 `shouldBe` GL [testBoss, testEmp1, testEmp2] 21

    it "works with monoid" $ do
      sGL testEmp1 <> sGL testEmp2 <> sGL testBoss `shouldBe` GL [testEmp1, testEmp2, testBoss] 21

    it "works with moreFun" $ do
      moreFun (sGL testEmp1) (sGL testEmp2) `shouldBe` sGL testEmp2
      moreFun (sGL testEmp2) (sGL testEmp1) `shouldBe` sGL testEmp2
      moreFun (sGL testEmp2) (sGL testBoss) `shouldBe` sGL testBoss
      moreFun testGL1n2 (sGL testBoss) `shouldBe` testGL1n2

  describe "ex2" $ do
    it "works" $ do
      countEmployees testCompany `shouldBe` 8
      countEmployees testCompany2 `shouldBe` 8
      sumEmployeeFun testCompany `shouldBe` 46
      sumEmployeeFun testCompany2 `shouldBe` 47
      employeeNames testCompany `shouldBe` ["Stan", "Bob", "Joe", "John", "Sue", "Fred", "Sarah", "Sam"]
      employeeNames testCompany2 `shouldBe` ["Stan", "Bob", "Joe", "John", "Sue", "Fred", "Sarah", "Sam"]

  describe "ex3" $ do
    it "works with empty" $ do
      nextLevel testBoss [] `shouldBe` (sGL testBoss, mempty)
      nextLevel testBoss [(mempty, mempty)] `shouldBe` (sGL testBoss, mempty)

    it "works with single entry" $ do
      nextLevel testBoss [(sGL testEmp1, mempty)] `shouldBe` (sGL testBoss, sGL testEmp1)
      nextLevel testBoss [(mempty, sGL testEmp2)] `shouldBe` (sGL testBoss <> sGL testEmp2, mempty)
      nextLevel testBoss [(sGL testEmp1, sGL testEmp2)] `shouldBe` (sGL testBoss <> sGL testEmp2, sGL testEmp1)

    it "works with multiple entries" $ do
      nextLevel testBoss [(sGL testEmp1, sGL testEmp2), (sGL testEmp3, sGL testEmp4)] `shouldBe`
        (sGL testBoss <> sGL testEmp2 <> sGL testEmp4, sGL testEmp1 <> sGL testEmp3)

  describe "ex4" $ do
    it "works" $ do
      maxFun (Node testBoss []) `shouldBe` GL [ testBoss ] 9
      maxFun testCompany `shouldBe` GL [ Emp {empName = "Bob", empFun = 2}
                                       , Emp {empName = "John", empFun = 1}
                                       , Emp {empName = "Sue", empFun = 5}
                                       , Emp {empName = "Sarah", empFun = 17}] 25
      maxFun testCompany2 `shouldBe` GL [ Emp {empName = "Bob", empFun = 3}
                                       , Emp {empName = "John", empFun = 1}
                                       , Emp {empName = "Sue", empFun = 5}
                                       , Emp {empName = "Sarah", empFun = 17}] 26

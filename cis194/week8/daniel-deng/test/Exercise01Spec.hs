module CIS194.Homework08.Exercise01Spec where

import CIS194.Homework08.Employee (Employee(..), GuestList(..))
import CIS194.Homework08.Exercise01

import Test.Tasty.Hspec

mkGL :: [Employee] -> GuestList
mkGL = mconcat . fmap singleGL
     where
       singleGL = GL . pure <*> empFun

e0 :: Employee
e0 = Emp "Haskell Curry" 100

e1 :: Employee
e1 = Emp "Haskell Teacher" 80

e2 :: Employee
e2 = Emp "Haskell Developer" 50

e3 :: Employee
e3 = Emp "Haskell Learner" 50

spec_glCons :: Spec
spec_glCons =

  it "returns the expected GuestList" $ do
    glCons e1 mempty `shouldBe` GL [e1] 80
    glCons e3 (mkGL [e2, e1, e0]) `shouldBe` GL [e3, e2, e1, e0] 280

spec_moreFun :: Spec
spec_moreFun =

  it "returns the GuestList with a higher totalFun score" $ do
    let gl01 = mkGL [e0, e1] -- 180
    let gl123 = mkGL [e1, e2, e3] -- 180
    let gl23 = mkGL [e2, e3] -- 100
    moreFun gl01 gl23 `shouldBe` gl01
    moreFun gl01 gl123 `shouldBe` gl123 

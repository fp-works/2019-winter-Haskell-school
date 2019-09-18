module CIS194.Homework05.Exercise05Spec where

-- import CIS194.Homework05.Exercise03 (Expr(..))
import CIS194.Homework05.StackVM (StackExp(..))
import CIS194.Homework05.Exercise05

import Test.Tasty.Hspec

spec_compile :: Spec
spec_compile =

  it "as an instance of Expr" $
    compile "(2+3)*4" `shouldBe` Just [PushI 2, PushI 3, Add, PushI 4, Mul]

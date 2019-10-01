import Employee
import Party
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "Party" $ do
      describe "exercie 1" $ do
        let gl1 = GL [Emp {empName = "Emp 1", empFun = 10}] 10
        let gl2 = GL [Emp {empName = "Emp 2", empFun = 15}] 15
        describe "1.1" $ do
          it "should return a correct guestList with an added Employee" $ do
            let initEL = [Emp {empName = "John", empFun = 8}]
            let initGL = GL initEL 8
            let newEmp = Emp {empName = "Tien", empFun = 15}
            glCons newEmp initGL `shouldBe` (GL (newEmp : initEL) 23)
        describe "1.2" $ do
          it "should do mconcat to add to GuestLists" $ do
            mconcat [gl1, gl2] `shouldBe`
              (GL
                 [ Emp {empName = "Emp 1", empFun = 10}
                 , Emp {empName = "Emp 2", empFun = 15}
                 ]
                 25)
        describe "1.3" $ do
          it "should return more fun guestList" $ do
            moreFun gl1 gl2 `shouldBe` gl2

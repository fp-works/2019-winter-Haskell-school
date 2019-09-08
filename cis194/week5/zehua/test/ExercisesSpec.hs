module ExercisesSpec where

import qualified Data.Map   as M
import           Exercises
import           ExprT      as E
import           StackVM    as S
import           Test.Hspec

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs e = e $ M.fromList vs

spec :: Spec
spec = do
  describe "ex1" $ do
    it "works" $ do
      mapM_ (\(e, v) -> eval e `shouldBe` v)
        [ (E.Lit 3, 3)
        , (E.Add (E.Lit 2) (E.Lit 3), 5)
        , (E.Mul (E.Lit 2) (E.Lit 3), 6)
        , (E.Mul (E.Add (E.Lit 2) (E.Lit 3)) (E.Lit 4), 20)
        , (E.Add (E.Mul (E.Lit 2) (E.Lit 3)) (E.Mul (E.Lit 4) (E.Add (E.Lit 5) (E.Lit 6))), 50) ]

  describe "ex2" $ do
    it "returns Nothing for invalid exprs" $ do
      mapM_ (\(e, v) -> evalStr e `shouldBe` v)
        [ ("2+3*", Nothing)
        , ("2+3*+", Nothing)
        , ("+", Nothing)
        , ("*", Nothing)
        , ("*4", Nothing) ]

    it "evaluates" $ do
      mapM_ (\(e, v) -> evalStr e `shouldBe` v)
        [ ("(2+3)*4", Just 20)
        , ("2+3*4", Just 14)
        , ("2+3*4*5", Just 62)
        , ("(2+3)*(4+5)", Just 45) ]

  describe "ex3" $ do
    it "works" $ do
      mapM_ (\(e, v) -> reify e `shouldBe` v)
        [ (lit 3, E.Lit 3)
        , (add (lit 2) (lit 3), E.Add (E.Lit 2) (E.Lit 3))
        , (mul (lit 2) (lit 3), E.Mul (E.Lit 2) (E.Lit 3))
        , (mul (add (lit 2) (lit 3)) (lit 4), E.Mul (E.Add (E.Lit 2) (E.Lit 3)) (E.Lit 4))
        , (add (mul (lit 2) (lit 3)) (mul (lit 4) (add (lit 5) (lit 6))),
           E.Add (E.Mul (E.Lit 2) (E.Lit 3)) (E.Mul (E.Lit 4) (E.Add (E.Lit 5) (E.Lit 6)))) ]

  describe "ex4" $ do
    it "works for Bool" $ do
      mapM_ (\(e, v) -> e `shouldBe` v)
        [ (lit 3, True)
        , (lit 0, False)
        , (lit (-2), False)
        , (add (lit 2) (lit 2), True)
        , (add (lit 2) (lit 0), True)
        , (add (lit 0) (lit 1), True)
        , (add (lit 0) (lit 0), False)
        , (mul (lit 2) (lit 2), True)
        , (mul (lit 2) (lit 0), False)
        , (mul (lit 0) (lit 1), False)
        , (mul (lit 0) (lit 0), False)
        , (mul (add (lit 2) (lit 0)) (lit 4), True)
        , (add (mul (lit 0) (lit 3)) (mul (lit 0) (add (lit 5) (lit 0))), False) ]

    it "works for MinMax" $ do
      mapM_ (\(e, v) -> e `shouldBe` v)
        [ (lit 3, MinMax 3)
        , (lit 0, MinMax 0)
        , (add (lit 3) (lit 2), MinMax 3)
        , (mul (lit 3) (lit 2), MinMax 2)
        , (mul (add (lit 2) (lit 0)) (lit 4), MinMax 2)
        , (add (mul (lit 6) (lit 3)) (mul (lit 5) (add (lit 6) (lit 4))), MinMax 5) ]

    it "works for Mod7" $ do
      mapM_ (\(e, v) -> e `shouldBe` v)
        [ (lit 0, Mod7 0)
        , (lit 13, Mod7 6)
        , (add (lit 6) (lit 3), Mod7 2)
        , (mul (lit 3) (lit 4), Mod7 5)
        , (mul (add (lit 2) (lit 0)) (lit 4), Mod7 1)
        , (add (mul (lit 6) (lit 4)) (mul (lit 5) (add (lit 6) (lit 4))), Mod7 4) ]

  describe "ex5" $ do
    it "compiles" $ do
      mapM_ (\(e, v) -> compile e `shouldBe` Just v)
        [ ("5", [S.PushI 5])
        , ("2+3", [S.PushI 2, S.PushI 3, S.Add])
        , ("3*4", [S.PushI 3, S.PushI 4, S.Mul])
        , ("(2+3)*4", [S.PushI 2, S.PushI 3, S.Add, S.PushI 4, S.Mul])
        , ("2+3*4", [S.PushI 2, S.PushI 3, S.PushI 4, S.Mul, S.Add])
        , ("2+3*4*5", [S.PushI 2, S.PushI 3, S.PushI 4, S.PushI 5, S.Mul, S.Mul, S.Add])
        , ("(2+3)*(4+5)", [S.PushI 2, S.PushI 3, S.Add, S.PushI 4, S.PushI 5, S.Add, S.Mul]) ]

    it "executes" $ do
      mapM_ (\(e, v) -> fmap stackVM (compile e) `shouldBe` (Just $ Right $ IVal v))
        [ ("2+3", 5)
        , ("2+3+4+5", 14)
        , ("2*3*4*5", 120)
        , ("(2*3)+(4*5)+(5*6)", 56)
        , ("(2+3+4)*((4*6)+5)*(3+(2*7))", 4437) ]

  describe "ex6" $ do
    it "returns Nothing when var not found" $ do
      mapM_ (\(vs, e) -> withVars vs e `shouldBe` Nothing)
        [ ([("x", 6)],
           add (lit 3) (var "y"))
        , ([("x", 6) ],
           mul (var "x") (add (var "y") (var "x"))) ]

    it "computes with vars" $ do
      mapM_ (\(vs, e, v) -> withVars vs e `shouldBe` Just v)
        [ ([],
           add (lit 3) (lit 6),
           9)
        , ([("x", 6)],
           add (lit 3) (lit 6),
           9)
        , ([("x", 6)],
           add (lit 3) (var "x"),
           9)
        , ([("x", 6), ("y", 3)],
           mul (var "x") (add (var "y") (var "x")),
           54)
        , ([("x", 6), ("y", 3), ("z", 7)],
           mul (mul (lit 2) (var "z")) (add (var "y") (var "x")),
           126) ]


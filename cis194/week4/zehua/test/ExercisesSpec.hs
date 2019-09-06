module ExercisesSpec where

import           Control.Monad       (liftM2)
import           Data.Bool           (bool)
import           Data.Char           (isSpace, isUpper)
import           Data.Numbers.Primes (primes)
import           Exercises
import           Test.Hspec

isBalancedTree :: Tree a -> Bool
isBalancedTree Leaf = True
isBalancedTree (Node h l _ r) =
    bool (h == hl + 1 && hl - hr <= 1) (h == hr + 1 && hr - hl <= 1) (hl < hr)
  where
    hl = getHeight l
    hr = getHeight r

insertTree :: (Ord a) => Tree a -> a -> Tree a
insertTree Leaf a = Node 0 Leaf a Leaf
insertTree (Node h l n r) a
  | a < n     = Node h (insertTree l a) n r
  | otherwise = Node h l n (insertTree r a)

genPrimes :: Integer -> [Integer]
genPrimes n = takeWhile (<=2*n+1) . drop 1 $ primes

spec :: Spec
spec = do
  describe "ex1.fun1" $ do
    it "works with empty" $ do
      fun1' [] `shouldBe` fun1 []

    it "works with non-empty" $ do
      mapM_ (liftM2 shouldBe fun1' fun1)
        [ [1,3,4]
        , [1]
        , [2]
        , [1,2,3]
        , [1,3..11]
        , [2,4..20]
        , [3..100] ]

  describe "ex1.fun2" $ do
    it "works with 1" $ do
      fun2' 1 `shouldBe` fun2 1

    it "works with positive integers" $ do
      mapM_ (liftM2 shouldBe fun2' fun2)
        [2..10]

  describe "ex2" $ do
    it "works with empty" $ do
      foldTree ([] :: [Char]) `shouldBe` (Leaf :: Tree Char)

    it "works with one item" $ do
      foldTree "A" `shouldBe` (Node 0 Leaf 'A' Leaf)

    it "works with multiple items" $ do
      mapM_ (\l -> foldTree l `shouldSatisfy` isBalancedTree)
        [ "AB"
        , "ABCDEFG"
        , "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ]

  describe "ex3" $ do
    it "xor" $ do
      mapM_ (\(l, v) -> xor l `shouldBe` v)
      -- mapM_ (uncurry (shouldBe . xor))
        [ ([], False)
        , ([True], True)
        , ([True, False, True], False)
        , ([True, False, True, False], False)
        , ([True, False, True, False, False], False)
        , ([True, False, True, True, False, False], True) ]

    it "map" $ do
      mapM_ (liftM2 shouldBe (uncurry map') (uncurry map)) $
        zip [ (+1)
            , (*3) ]
            [ [1..100] :: [Integer]
            , [1..100] ]
      mapM_ (liftM2 shouldBe (uncurry map') (uncurry map)) $
        zip [ even
            , odd ]
            [ [1..100] :: [Integer]
            , [1..200] :: [Integer] ]
      mapM_ (liftM2 shouldBe (uncurry map') (uncurry map)) $
        zip [ isUpper
            , isSpace ]
            [ "AbCDe"
            , "A0Bcd " ]

    it "foldl" $ do
      myFoldl (flip (:)) [] "ABCD"
        `shouldBe`
          foldl (flip (:)) [] "ABCD"

      myFoldl xor2 False [True, False, True, True, False, False]
        `shouldBe`
          foldl xor2 False [True, False, True, True, False, False]

      myFoldl insertTree Leaf "ACBDEF"
        `shouldBe`
          foldl insertTree Leaf "ACBDEF"

  describe "ex4" $ do
    it "works" $ do
      mapM_ (liftM2 shouldBe sieveSundaram genPrimes)
        [4,10,100,1000]

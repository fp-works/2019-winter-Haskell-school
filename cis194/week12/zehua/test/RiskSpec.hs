module RiskSpec where

import           Control.Monad        (replicateM_)
import           Control.Monad.Random (evalRand, evalRandIO)
import           Data.Ix              (inRange)
import           Risk
import           System.Random        (RandomGen (genRange, next, split))
import           Test.Hspec


-- a mock to simulate random generation with a pre-defined sequence of "random" numbers
-- we only support one single list of values, once used up _|_ will be returned
newtype MockRandomGen = MockRandomGen { getValues :: [Int] }
  deriving Show

instance RandomGen MockRandomGen where
  next g = case getValues g of
             (n:ns) -> (n, MockRandomGen ns)
             _      -> undefined
  genRange _ = (1, 6)
  split = undefined

mkMockRandomGen :: [Int] -> MockRandomGen
-- inserting [1,1,1,1] in between each desired random output as `random` calls `next` multiple times
-- and it so happens that it takes 5 calls to `next` for it to be happy when genRange is (1,6)
-- see https://hackage.haskell.org/package/random-1.1/docs/src/System.Random.html#randomIvalInteger
mkMockRandomGen = MockRandomGen . concat . map (\x -> [1,1,1,1,x])

rollsShouldBeValid :: [DieValue] -> Int -> Expectation
rollsShouldBeValid rs n = do
  length rs `shouldBe` n
  -- ensure in range
  mapM_ (\a -> a `shouldSatisfy` inRange (1, 6) . unDV ) rs
  -- ensure sorted desc
  mapM_ (\a -> a `shouldSatisfy` uncurry (>=)) $ zip rs (tail rs)

bf :: Army -> Army -> Battlefield
bf a d = Battlefield { attackers = a, defenders = d }


spec :: Spec
spec = do
  describe "ex1" $ do
    it "works for dieNSorted via MockRandomGen" $ do
      (flip evalRand) (mkMockRandomGen [1,2,3,4,5,6]) $ do
        r1 <- dieNSorted 1
        r2 <- dieNSorted 3
        r3 <- dieNSorted 2
        return (\() -> do
          r1 `shouldBe` [1]
          r2 `shouldBe` [4,3,2]
          r3 `shouldBe` [6,5]
          )

    it "works for dieNSorted with StdGen via evalRandIO" $ do
      -- repeating more times in a hope to cover the full range
      replicateM_ 100 $ do
        actual <- evalRandIO $ dieNSorted 1
        actual `rollsShouldBeValid` 1
      replicateM_ 100 $ do
        actual <- evalRandIO $ dieNSorted 2
        actual `rollsShouldBeValid` 2
      replicateM_ 100 $ do
        actual <- evalRandIO $ dieNSorted 3
        actual `rollsShouldBeValid` 3

  describe "ex2" $ do
    it "works for updateBattlefield" $ do
      updateBattlefield (bf 3 3) [(6, 5)] `shouldBe` bf 3 2
      updateBattlefield (bf 3 3) [(6, 1)] `shouldBe` bf 3 2
      updateBattlefield (bf 3 3) [(3, 3)] `shouldBe` bf 2 3
      updateBattlefield (bf 3 3) [(3, 6)] `shouldBe` bf 2 3
      updateBattlefield (bf 3 3) [(6, 5), (5, 3)] `shouldBe` bf 3 1
      updateBattlefield (bf 3 3) [(6, 5), (5, 3), (3, 2)] `shouldBe` bf 3 0
      updateBattlefield (bf 3 3) [(5, 5), (1, 2)] `shouldBe` bf 1 3
      updateBattlefield (bf 3 3) [(5, 5), (3, 3), (1, 2)] `shouldBe` bf 0 3
      updateBattlefield (bf 3 3) [(6, 5), (3, 3)] `shouldBe` bf 2 2
      updateBattlefield (bf 3 3) [(6, 5), (3, 3), (3, 2)] `shouldBe` bf 2 1
      updateBattlefield (bf 3 3) [(6, 5), (3, 3), (1, 2)] `shouldBe` bf 1 2
      updateBattlefield (bf 3 3) [(1,3), (6, 5), (3, 3), (1, 2), (4, 1), (6, 1)] `shouldBe` bf 0 0

    it "works for dispatchAttackers" $ do
      dispatchAttackers (bf 0 3) `shouldBe` 0
      dispatchAttackers (bf 1 3) `shouldBe` 0
      dispatchAttackers (bf 2 3) `shouldBe` 1
      dispatchAttackers (bf 3 3) `shouldBe` 2
      dispatchAttackers (bf 4 3) `shouldBe` 3
      dispatchAttackers (bf 5 3) `shouldBe` 3
      dispatchAttackers (bf 10 3) `shouldBe` 3

    it "works for dispatchDefenders" $ do
      dispatchDefenders (bf 4 0) `shouldBe` 0
      dispatchDefenders (bf 4 1) `shouldBe` 1
      dispatchDefenders (bf 4 2) `shouldBe` 2
      dispatchDefenders (bf 4 3) `shouldBe` 2
      dispatchDefenders (bf 4 9) `shouldBe` 2

    it "works for battle via MockRandomGen" $ do
      (flip evalRand) (mkMockRandomGen [1,2,3,4,5, 6,3,4,5,3, 6,3,4,3,6, 3,1,4, 1,3,2, 6, 5]) $ do
        r1 <- battle $ bf 4 4 -- a: 3,2,1 d: 5,4
        r2 <- battle $ bf 4 4 -- a: 6,4,3 d: 5,3
        r3 <- battle $ bf 4 4 -- a: 6,4,3 d: 6,3
        r4 <- battle $ bf 2 4 -- a: 3 d: 4,1
        r5 <- battle $ bf 3 1 -- a: 3,1 d: 2
        r6 <- battle $ bf 2 0 -- a: 6 d:
        r7 <- battle $ bf 1 1 -- a:   d: 5
        return (\() -> do
          r1 `shouldBe` bf 2 4
          r2 `shouldBe` bf 4 2
          r3 `shouldBe` bf 3 3
          r4 `shouldBe` bf 1 4
          r5 `shouldBe` bf 3 0
          r6 `shouldBe` bf 2 0
          r7 `shouldBe` bf 1 1
          )

  describe "ex3" $ do
    it "works for invade via MockRandomGen" $ do
      (flip evalRand) (mkMockRandomGen [1,2,3,4,5, 6,3,4, 5,3,2, 6,6,4
                                       ,3,6,4,5,3
                                       ,3,2,4,1, 1,2
                                       ,5,6
                                       ]) $ do
        -- a: 3,2,1 d: 5,4 -> bf 2 4
        -- a: 6 d: 4,3 -> bf 2 3
        -- a: 5 d: 3,2 -> bf 2 2
        -- a: 6 d: 6,4 -> bf 1 2
        r1 <- invade $ bf 4 4
        -- a: 6,4,3 d: 5,3 -> bf 4 0
        r2 <- invade $ bf 4 2
        -- a: 3,2 d: 4,1 -> bf 2 1
        -- a: 1 d: 2 -> bf 1 1
        r3 <- invade $ bf 3 2
        -- a: 5 d: 6 -> bf 1 1
        r4 <- invade $ bf 2 1
        -- no changes
        r5 <- invade $ bf 2 0
        -- no changes
        r6 <- invade $ bf 1 1
        -- no changes
        r7 <- invade $ bf 1 0
        return (\() -> do
          r1 `shouldBe` bf 1 2
          r2 `shouldBe` bf 4 0
          r3 `shouldBe` bf 1 1
          r4 `shouldBe` bf 1 1
          r5 `shouldBe` bf 2 0
          r6 `shouldBe` bf 1 1
          r7 `shouldBe` bf 1 0
          )

  describe "ex4" $ do
    it "works for invadeSucceeded" $ do
      invadeSucceeded (bf 1 1) `shouldBe` False
      invadeSucceeded (bf 2 1) `shouldBe` False
      invadeSucceeded (bf 2 0) `shouldBe` True
      invadeSucceeded (bf 1 0) `shouldBe` True

    it "works for invadeAndCheck via MockRandomGen" $ do
      (flip evalRand) (mkMockRandomGen [1,2,3,4,5, 6,3,4, 5,3,2, 6,6,4
                                       ,3,6,4,5,3
                                       ,3,2,4,1, 1,2
                                       ,5,6
                                       ]) $ do
        -- a: 3,2,1 d: 5,4 -> bf 2 4
        -- a: 6 d: 4,3 -> bf 2 3
        -- a: 5 d: 3,2 -> bf 2 2
        -- a: 6 d: 6,4 -> bf 1 2
        r1 <- invadeAndCheck $ bf 4 4
        -- a: 6,4,3 d: 5,3 -> bf 4 0
        r2 <- invadeAndCheck $ bf 4 2
        -- no changes
        r5 <- invadeAndCheck $ bf 2 0
        -- no changes
        r6 <- invadeAndCheck $ bf 1 1
        return (\() -> do
          r1 `shouldBe` False
          r2 `shouldBe` True
          r5 `shouldBe` True
          r6 `shouldBe` False
          )

    it "works for average" $ do
      average 2 [True, False] `shouldBe` 1 / 2
      average 2 [False, False] `shouldBe` 0
      average 2 [True, True] `shouldBe` 1.0
      average 3 [False, False, False] `shouldBe` 0.0
      average 3 [True, False, False] `shouldBe` 1 / 3
      average 3 [True, True, False] `shouldBe` 2.0 / 3
      average 3 [True, True, True] `shouldBe` 1

    it "works for successProbN via MockRandomGen" $ do
      (flip evalRand) (mkMockRandomGen [1,2,3,4,5, 6,3,4, 5,3,1, 4,3
                                       ,3,6,4,5,3, 1,2,2,2, 2,4,3
                                       ,3,2,4,3,5, 4,3,6,2
                                       ]) $ do
        -- round 1 True
        -- a: 3,2,1 d: 5,4 -> bf 2 3
        -- a: 6 d: 4,3 -> bf 2 2
        -- a: 5 d: 3,1 -> bf 2 1
        -- a: 4 d: 3 -> bf 2 0
        -- round 2 True
        -- a: 6,4,3 d: 5,3 -> bf 4 1
        -- a: 2,2,1 d: 2 -> bf 3 1
        -- a: 4,2 d: 3 -> bf 3 0
        -- round 3 False
        -- a: 4,3,2 d: 5,3 -> bf 2 3
        -- a: 4,3 d: 6,2 -> bf 1 2
        r1 <- successProbN 3 $ bf 4 3
        return (\() -> do
          r1 `shouldBe` 2 / 3
          )

    it "works for successProbN via StdGen" $ do
      actual <- evalRandIO $ successProbN 50 (bf 4 4)
      actual `shouldSatisfy` (&&) <$> (>=0) <*> (<=1)

    it "works for successProb via StdGen" $ do
      actual <- evalRandIO $ successProb (bf 4 4)
      actual `shouldSatisfy` (&&) <$> (>=0) <*> (<=1)

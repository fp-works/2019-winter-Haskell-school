import Test.Hspec

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = [] -- base case, 0
hanoi 1 a b _ = [(a,b)] -- base case, 1
hanoi n a b c = (hanoi (n-1) a c b) ++ [(a,b)] ++ (hanoi (n-1) c b a)


{-
- Concrete Math Exercise 1.17
- (1) If Wn is the minimum number of moves needed to transfer a tower of n
- disks from one peg to another when there are four pegs instead of three,
- show that
-
-   W_{n(n+1)/2} <= 2 * W_{n(n-1)/2} + T_n, n > 0
-
- Proof:
- To move n(n+1)/2 disks from peg a to peg b, we can move n(n-1)/2 disks from a
- to c using b and d as temporary pegs, move the remaining n disks on peg a to 
- peg b using peg d as temporary peg (can't use c because of illegal moves), then
- move all disks on peg c to peg b using peg a and d as temporary pegs. This yield
- exactly the above inequality. <>
-
- (2) Use this to find a closed form f(n) such that
-
-   W_{n(n+1)/2} <= f(n), n >= 0
-
- Proof:
- Define f as follows:
-
-   f(n) = 2*f(n-1) + T_n, n > 0
-   f(0) = 0
-
- base case:
-   f(0) = 0
-   W_0 = 0 <= f(0) = 0
- induction case:
-   f(n) = 2*f(n-1) + T_n >= 2*W{n(n-1)/2} + T_n >= W{n(n+1)/2}
-
- closed form of f(n) (left as exercise to the reader) should be:
-
- f(0) = 0
- f(n) = 2^n * (n - 1) + 1, n > 0
-
- <>
-
-
- For arbitrary W_n (n > 2), find the maximum p such that p*(p+1)/2 <= n,
- then
-   W_n <= W_{p(p-1)/2} + T_{n-p(p-1)/2}
-
- The frame-stewart conjecture suggests that W_n = W_{p(p-1)/2} + T_{n-p(p-1)/2},
- this was proven in 2014 by this paper:
-
-   https://scholar.google.com/scholar?cluster=7454289741749105922
-
-}

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 n a b c d = (hanoi4 p' a c b d) ++ (hanoi (n - p') a b d) ++ (hanoi4 p' c b a d)
  where p' = p * (p - 1) `div` 2
        p = floor ((sqrt (fromIntegral (1 + 8 * n)) - 1) / 2)

main :: IO ()
main = hspec $ do
  describe "hanoi-3" $ do
    it "returns correct moves with 0 disc" $ do
      (hanoi 0 "a" "b" "c") `shouldBe` []
    it "returns correct moves with 1 disc" $ do
      (hanoi 1 "a" "b" "c") `shouldBe` [("a", "b")]
    it "returns correct moves with 2 disc" $ do
      (hanoi 2 "a" "b" "c") `shouldBe` [("a","c"), ("a","b"), ("c","b")]
    it "returns correct moves with 3 disc" $ do
      (hanoi 3 "a" "b" "c") `shouldBe` [("a", "b"), ("a", "c"), ("b", "c"),
                                        ("a", "b"), ("c", "a"), ("c", "b"),
                                        ("a", "b")]
  describe "hanoi-4" $ do
    it "returns correct moves with 0 disc" $ do
      (hanoi4 0 "a" "b" "c" "d") `shouldBe` []
    it "returns correct moves with 1 disc" $ do
      (hanoi4 1 "a" "b" "c" "d") `shouldBe` [("a", "b")]
    it "returns correct number of moves with 2 disc" $ do
      (length (hanoi4 2 "a" "b" "c" "d")) `shouldBe` 3
    it "returns correct number of moves with 3 disc" $ do
      (length (hanoi4 3 "a" "b" "c" "d")) `shouldBe` 5
    it "returns correct number of moves with 15 disc" $ do
      (length (hanoi4 15 "a" "b" "c" "d")) `shouldBe` 129


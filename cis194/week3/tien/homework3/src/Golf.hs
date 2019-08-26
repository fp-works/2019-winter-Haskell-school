module Golf where

-- Exercise 1 --
everyNth :: Integral b => b -> [(a, b)] -> [a]
everyNth n = fst . unzip . filter ((== 0) . (`mod` n) . snd)

skips :: [a] -> [[a]]
skips [] = []
skips arr =  (`everyNth` pairs) <$> indexArr
  where
    indexArr = [1 .. length arr]
    pairs = zip arr indexArr

-- Exercise 2 --
-- findMaxima (x:y:z:xs)
--   | y > x && y > z = [y]
--   | otherwise = findMaxima (y : z : xs)
-- localMaxima :: [Integer] -> [Integer]
-- localMaxima [] = []
-- localMaxima [_] = []
-- localMaxima [_, _] = []
-- localMaxima arr = foldl (\acc x-> )

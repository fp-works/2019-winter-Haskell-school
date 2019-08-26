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
localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs)
  | y > x && y > z = y : localMaxima (y : z : xs)
  | otherwise = localMaxima (y : z : xs)
localMaxima _ = []

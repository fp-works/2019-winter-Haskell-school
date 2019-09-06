module Exercise3
  ( xor
  , map'
  , myFoldl
  ) where

xor :: [Bool] -> Bool
xor bs = foldr (==) True bs

-- foldr does not work ??!!
map' :: (a -> b) -> [a] -> [b]
map' _ []     = []
map' f (n:ns) = foldl (\acc elem -> acc ++ [f elem]) [f n] ns

-- https://stackoverflow.com/questions/6172004/writing-foldl-using-foldr
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f_aba init bs = foldr (\b g x -> g (f_aba x b)) id bs init


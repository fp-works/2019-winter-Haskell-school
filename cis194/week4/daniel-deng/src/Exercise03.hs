module CIS194.Homework04.Exercise03 (xor, map' {- , myFoldl -}) where

xor :: [Bool] -> Bool
xor = foldr (/=) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

{-
foldr f z [x1, x2, ..., xn] == x1 ‘f‘ (x2 ‘f‘ ... (xn ‘f‘ z)...)
foldl f z [x1, x2, ..., xn] == (...((z ‘f‘ x1) ‘f‘ x2) ‘f‘...) ‘f‘ xn

-- still thinking about how to implement foldl with foldr
myFoldl :: (a -> b -> a) -> a -> [b] -> a
-}

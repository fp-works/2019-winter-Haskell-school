module Homework4 where

import           Data.Bool

-- Exercise 1 --
-- 1.1 --
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . (subtract 2 <$>) . filter even

-- 1.2 --
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (> 1) . iterate configInputs
  where
    configInputs = bool <$> (+ 1) . (* 3) <*> flip div 2 <*> even

-- Exercise 2 --
data Tree a
  = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

checkDepth :: Eq a => Tree a -> Integer
checkDepth Leaf = (0 :: Integer)
checkDepth (Node _ Leaf _ Leaf) = 1
checkDepth (Node _ Leaf _ right@(Node _ _ _ _)) = 1 + checkDepth right
checkDepth (Node _ left@(Node _ _ _ _) _ Leaf) = 1 + checkDepth left
checkDepth (Node _ left@(Node _ _ _ _) _ right@(Node _ _ _ _)) =
  1 + checkDepth left + checkDepth right

insert :: Eq t => t -> Tree t -> Tree t
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node i left c Leaf) = Node i left c (Node (i + 1) Leaf x Leaf)
insert x (Node i Leaf c right) = Node i (Node (i + 1) Leaf x Leaf) c right
insert x (Node i left c right)
  | (checkDepth left) > (checkDepth right) = Node i left c (insert x right)
  | otherwise = Node i (insert x left) c right

foldTree :: (Foldable t, Eq a) => t a -> Tree a
foldTree = foldr insert Leaf

-- Exercise 3.1 --
xor :: [Bool] -> Bool
xor = odd . foldr (\x acc -> acc + (bool 0 1 x)) (0 :: Integer)

-- Exercise 3.2 --
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

-- Exercise 3.3 --
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\x acc -> f acc x) base (reverse xs)

-- Exercise 4 --
deletedSunDaram :: Integer -> [Integer]
deletedSunDaram n =
  [i + j + 2 * i * j | i <- [1 .. n], j <- [1 .. i], i + j + 2 * i * j < n]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = [2 * i + 1 | i <- [1 .. n], notElem i (deletedSunDaram n)]

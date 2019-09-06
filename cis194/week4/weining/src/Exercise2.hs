module Exercise2
  ( Tree(..)
  , foldTree
  , height
  ) where


data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

endNode :: a -> Tree a
endNode v = Node 0 Leaf v Leaf

-- use a partition algorithm to create the tree then calculate
-- the height for each node (still N.logN)
foldTree :: [a] -> Tree a
foldTree []     = Leaf
foldTree (x:xs) =
  let (firstHalf, secondHalf) = splitAt (length xs `div` 2) xs
      t = Node 0 (foldTree firstHalf) x (foldTree secondHalf)
  in calculateHeight t

height :: Tree a -> Integer
height Leaf           = 0
height (Node h _ _ _) = h

calculateHeight :: Tree a -> Tree a
calculateHeight Leaf = Leaf
calculateHeight n@(Node _ Leaf _ Leaf) = n
calculateHeight (Node h l v r) = Node (1 + max (height l) (height r)) l v r

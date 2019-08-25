module Exercises where

import           Control.Monad (ap)
import           Data.Bool (bool)
import           Data.Char (isSpace)
import           Data.List (sort, transpose)
-- import           Data.Tuple.Utils -- from stackage

-- ex1

-- helper function adapted from https://stackoverflow.com/a/2028218
-- doing `drop n xs` instead of `drop (n-1) xs` to save 4 chars :)
--   which requires passing in 0-based `zip [0..]` instead of `zip [1..]`
-- keeping the name `every` instead of a short one-character name, e.g., `e`
-- 73 chars
every :: (Int, [a]) -> [a]
every (n, xs) = case drop n xs of
              (y:ys) -> y : every (n, ys)
              []     -> []

-- 85 chars of every implementation based on iterate and splitAt
-- (\n -> (\l -> map last . takeWhile (not . null) . map fst . tail . iterate (splitAt n . snd) . ([],) $ l)) 2 [1,2,3,4,5,6]
-- derving free version:
-- \n -> iterate (splitAt n . snd)
--   === \n -> iterate ( (.snd) (splitAt n) )
--   === \n -> iterate (.snd) (splitAt n)
--   === iterate (.snd) splitAt
--
-- \n -> func1 n . func2 ===  flip (flip func1 . func2)
-- \n -> (iterate (.snd) splitAt) n . ([],)
--   === \n -> (flip (flip (iterate (.snd) splitAt) . ([],) )) n
--
-- func1 . func2 n == (func1 .) . func2 $ n
-- map last . takeWhile (not . null) . map fst . tail     .      (iterate (.snd) splitAt) n  ===
-- (map last . takeWhile (not . null) . map fst . tail .)    .    (iterate (.snd) splitAt) $ n
--
-- final point free version:
-- every = ((map last . takeWhile (not . null) . map fst . tail) .) . flip (flip (iterate . (.snd) . splitAt) . ([],) )

skips :: [a] -> [[a]]
-- infinite list first then cut off
-- skips = takeWhile (not . null) . map every . zip [0..] . repeat
-- dropWhileEnd does not seem to work on infinite list :|
-- skips = dropWhileEnd null . map every . zip [0..] . repeat

-- <*> S combinator on reader monad to implement s x y z = (x z) (y z)
--   https://kseo.github.io/posts/2016-12-24-reader-monad-and-ski-combinators.html
-- skips = map every . zip [0..] . (flip replicate <*> length)
-- skips = take . length <*> map every . zip [0..] . repeat

-- seems to be the most readable
-- 44 + 73 = 117 chars
skips l = map every . zip [0..] $ replicate (length l) l


-- ex2
localMaxima :: [Integer] -> [Integer]
-- using drop avoids having to check the edge cases of length <= 2
--localMaxima []    = []
--localMaxima [_]   = []
--localMaxima [_,_] = []
-- 78 chars
localMaxima =
    -- snd3 from Data.Tuple.Utils is not from haskell-platform :|
    map (\(_, x, _) -> x)
    . filter (\(x, y, z) -> x < y && y > z)
    -- $ zip3 l (drop 1 l) (drop 2 $ l) l
    -- . (zip3 <$> id <*> drop 1 <*> drop 2)
    . (zip3 <*> drop 1 <*> drop 2)


-- ex3
histogram :: [Integer] -> String
{-
-- 147 chars
histogram = 
    (++ "==========\n0123456789\n")
    . unlines
    -- . dropWhile (== "          ")
    -- . dropWhile (null . strip)
    -- . dropWhile (all (==' '))
    . dropWhile (all isSpace)
    . transpose
    . map sort
    . map (\(i, l) -> map (\x -> if x == i then '*' else ' ') l)
    . zip [0..9]
    . repeat
-}
-- 127 chars
histogram =
    (++ "==========\n0123456789\n")
    . unlines
    . dropWhile (all isSpace)
    . transpose
    . map sort
    -- . (<*>) (
    . ap (
        -- (!!) is partial function, not good
        -- \i -> map $ (" *"!!) . fromEnum . (==i)
        -- map . (((" *"!!) . fromEnum) .) . (==)
        -- (\i -> map $ bool ' ' '*' . (==i))
        map . ((bool ' ' '*') .) . (==)
          <$> [0..9])
    . pure

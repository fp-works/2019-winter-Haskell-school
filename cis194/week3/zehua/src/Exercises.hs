module Exercises where

-- import           Control.Applicative (liftA2)
import           Control.Monad (ap)
import           Data.Bool (bool)
import           Data.Char (isSpace)
import           Data.List ({-group, -}sort, transpose)
import           Data.List.HT (sieve)
-- import           Data.List.Index (ifilter{-, imap-})
-- import           Data.Maybe (catMaybes)
-- import           Data.List.Utils (countElem) -- from MissingH @ stackage
import           Data.Tuple.Utils (snd3) -- from MissingH @ stackage

-- ex1

-- helper function adapted from https://stackoverflow.com/a/2028218
-- doing `drop n xs` instead of `drop (n-1) xs` to save 4 chars :)
--   which requires passing in 0-based `zip [0..]` instead of `zip [1..]`
-- keeping the name `every` instead of a short one-character name, e.g., `e`
-- 72 chars
every :: (Int, [a]) -> [a]
every (n, xs) = case drop n xs of
              (y:ys) -> y : every (n, ys)
              _      -> []

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
-- 44 + 72 = 116 chars
--skips l = map every . zip [0..] $ replicate (length l) l

-- utilizing list applicative to save calls to repeat
-- and use a curried version of every to avoid zip
-- everY = curry every
-- 65 chars
everY :: Int -> [a] -> [a]
everY n xs = case drop n xs of
              (y:ys) -> y : everY n ys
              _      -> []
-- reader applicative and list applicative
-- skips = take . length <*> (\l -> everY <$> [0..] <*> [l])
-- skips = take . length <*> ap (everY <$> [0..]) . pure
-- 65 + 40 = 105 chars
-- somehow non-point-free version is shorter due to [] vs pure
--  and probably more intuitive due to <$> .. <*> .. vs ap (.. <$> ..)
-- skips l = take (length l) $ everY <$> [0..] <*> [l]
-- use liftA2 instead of .. <$> .. <*> ..
--  still 1 char longer than non-point-free, but as intuitive now
-- skips = take . length <*> liftA2 everY [0..] . pure
-- 65 + 40 = 105 chars
-- skips l = take (length l) $ liftA2 everY [0..] [l]
-- 65 + 38 = 103 chars
-- skips = liftA2 everY (map fst . zip [1..]) pure
-- 65 + 37 = 102 chars
-- skips l = liftA2 everY [0..(length l)-1] [l]
-- 65 + 33 = 100 chars
-- skips l = flip everY l <$> [1..length l]
-- 65 + 40 = 105 chars
-- skips = fmap . (flip sieve) <*> map fst . zip [1..]

-- using sieve from Data.List.HT
{-
every = liftA2 (.) sieve (drop . (-1 +))
every = (.) <$> sieve <*> drop . (-1 +))
every n l = sieve n (drop (n-1) l)
-}
-- 55 chars point free every
-- skips l = flip (liftA2 (.) sieve (drop . (-1 +))) l <$> [1..length l]
-- 46 chars
-- skips l = (\n -> sieve n . drop (n-1) $ l) <$> [1..length l]
skips l = (\n -> sieve n (drop (n-1) l)) <$> [1..length l]

-- using ifilter from Data.List.Index to implement every
-- every n = ifilter (\i _ -> (i+1) `mod` n == 0)
-- 60 chars
-- skips l = liftA2 (\n -> ifilter (\i _ -> mod (i+1) n == 0)) [1..length l] [l]
-- 73 chars point free of l
-- skips = liftA2 (liftA2 (\n -> ifilter (\i _ -> mod (i+1) n == 0)))  (map fst . zip [1..]) pure
-- 70 chars point free in ifilter
-- skips l = liftA2 (\n -> ifilter (const . ((==0) . (`mod` n) . (+1)))) [1..length l] [l]

{-
each column includes the value in rows that are factors of the current index,
and then filter the empty cells in each row
1234567890ABCDEF
 2 4 6 8 0 B D F
  3  6  9  B  E
   4   8   B   F
    5    0    E
     6     B
      7      D
       8       F
-}
{-
-- 114 chars
skips = takeWhile (not . null)
      . map catMaybes
      . transpose
      . imap (\i -> imap
                      -- (flip ((. ((==0) . ((i+1) `mod`) . (+1))) . bool Nothing ))
                      (\j a -> (bool Nothing a (mod (i+1) (j+1) == 0)))
                  . repeat . Just)
-}
{-
-- 109 chars
skips l = map catMaybes
        . transpose
        . imap (\i -> imap
                        (\j a -> (bool Nothing a (mod (i+1) (j+1) == 0)))
                    . replicate (length l) . Just)
        $ l
-}
--skips l = liftA2 everY [0..(length l)-1] [l]
--skips l = map every . zip [0..] $ replicate (length l) l
-- 67 chars
-- skips l = imap (\i -> ifilter (\j _ -> mod (j+1) (i+1) == 0)) $ replicate (length l) l


-- ex2
localMaxima :: [Integer] -> [Integer]
-- using drop avoids having to check the edge cases of length <= 2
--localMaxima []    = []
--localMaxima [_]   = []
--localMaxima [_,_] = []
-- 69 chars
localMaxima =
    map snd3
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
    -- liftA2 version is 1 char longer
    -- . liftA2
    --    (map . ((bool ' ' '*') .) . (==))
    --    [0..9]
    . pure

-- 137 chars using sort and group
-- (++ "==========\n0123456789\n") . unlines . map (map (bool '*' ' ' . null)) . reverse . drop 1 . takeWhile (any null) . iterate (map (drop 1)) . group . sort . (++ [0..9])
{-
histogram =
    (++ "==========\n0123456789\n")
  . unlines
  . map (map (bool '*' ' ' . null)) -- draw it
  . reverse
  . drop 1     -- revert the effect of step 1 (remove one occurrence for each)
  . takeWhile (any null)
  . iterate (map (drop 1)) -- gradually reduce the size by 1 in each sublist
  . group      -- [[0], [1,1,1], [2], ... ]
  . sort
  . (++ [0..9]) -- add 1 occurrence for each number so that we get at least one entry for each
-}
-- 147 chars using countElem
{-
histogram =
    (++ "==========\n0123456789\n")
  . unlines
  . map (map (bool '*' ' ' . (<1))) -- draw it
  . reverse
  . takeWhile (any (>0)) -- stop when all 0 or lower
  . iterate (map (-1 +)) -- gradually reduce the size
  -- (\l -> map ((flip countElem) l) [0..9])
  . flip (map . flip countElem) [0..9] -- count occurrences of each number
-}

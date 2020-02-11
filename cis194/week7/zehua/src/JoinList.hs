module JoinList where


import           Scrabble
import           Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

-- ex1
tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
Empty +++ a     = a
a     +++ Empty = a
j1    +++ j2    = Append (m1 <> m2) j1 j2
  where
    m1 = tag j1
    m2 = tag j2


-- ex2
sizedTag :: (Sized m, Monoid m) => JoinList m a -> m
sizedTag = tag

getSizeFromTag :: (Sized m, Monoid m) => JoinList m a -> Int
getSizeFromTag = getSize . size . sizedTag

foldJoinList :: (Sized b, Monoid b) =>
  -- e :: _ Empty
  c ->
  -- i0 :: i a | i < 0
  (JoinList b a -> c) ->
  -- s0 :: i s@(Single _ _) with i == 0
  (JoinList b a -> c) ->
  -- s1 :: i s@(Single _ _) with i > 0
  (JoinList b a -> c) ->
  -- so :: i s@(Single _ _) with otherwise
  (JoinList b a -> c) ->
  -- a0ge :: i j@(Append _ j1 j2) with i >= size0
  (Int -> Int -> JoinList b a -> c) ->
  -- a1l :: i j@(Append _ j1 j2) with i < size1
  (Int -> Int -> JoinList b a -> c) ->
  -- a1e :: i j@(Append _ j1 j2) with i == size0
  (Int -> Int -> JoinList b a -> c) ->
  -- a1g :: i j@(Append _ j1 j2) with i > size1 (but < size0)
  (Int -> Int -> JoinList b a -> c) ->
  Int -> JoinList b a -> c
foldJoinList e _  _  _  _  _    _   _   _   _ Empty     = e
foldJoinList _ i0 _  _  _  _    _   _   _   i a | i < 0 = i0 a
foldJoinList _ _  s0 s1 so _    _   _   _   i s@(Single _ _)
  | i == 0    = s0 s
  | i > 0     = s1 s
  | otherwise = so s
foldJoinList _ _  _  _  _  a0ge a1l a1e a1g i j@(Append _ j1 _)
  | i >= size0  = a0ge size1 i j
  | i < size1   = a1l  size1 i j
  | i == size1  = a1e  size1 i j
  | otherwise   = a1g  size1 i j -- i > size1 and i < size0
  where
    size0 = getSizeFromTag j
    size1 = getSizeFromTag j1

indexJDirect :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJDirect _ Empty     = Nothing
indexJDirect i _ | i < 0 = Nothing
indexJDirect i (Single _ a)
  | i == 0    = Just a
  | otherwise = Nothing
indexJDirect i j@(Append _ j1 j2)
  | i >= size0 = Nothing
  | i < size1  = indexJDirect i j1
  | otherwise  = indexJDirect (i - size1) j2
  where
    size0 = getSizeFromTag j
    size1 = getSizeFromTag j1

indexJFold :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJFold = foldJoinList
    Nothing
    (const Nothing)
    s0 (const Nothing) (const Nothing)
    a0ge a1l a1ge a1ge
  where
    s0 (Single _ a) = Just a
    s0 _            = Nothing
    a0ge _     _ _               = Nothing
    a1l  _     i (Append _ j1 _) = indexJFold i j1
    a1l  _     _ _               = Nothing
    a1ge size1 i (Append _ _ j2) = indexJFold (i - size1) j2
    a1ge _     _ _               = Nothing

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ = indexJFold

dropJDirect :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJDirect _ Empty     = Empty
dropJDirect i a | i < 0 = a
dropJDirect i s@(Single _ _)
  | i == 0    = s
  | otherwise = Empty
dropJDirect i j@(Append _ j1 j2)
  | i >= size0 = Empty
  | i < size1  = let j1New   = dropJDirect i j1
                     sizeNew = (sizedTag j1New) <> (sizedTag j2)
                 in Append sizeNew j1New j2
  | otherwise  = dropJDirect (i - size1) j2
  where
    size0 = getSizeFromTag j
    size1 = getSizeFromTag j1

dropJFold :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJFold = foldJoinList
    Empty
    id
    id (const Empty) (const Empty)
    a0ge a1l a1ge a1ge
  where
    a0ge _     _ _                = Empty
    a1l  _     i (Append _ j1 j2) =
      let j1New   = dropJFold i j1
          sizeNew = (sizedTag j1New) <> (sizedTag j2)
      in Append sizeNew j1New j2
    a1l  _     _ _                = Empty
    a1ge size1 i (Append _ _ j2) = dropJFold (i - size1) j2
    a1ge _     _ _               = Empty

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ = dropJFold

takeJDirect :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJDirect _ Empty     = Empty
takeJDirect i _ | i < 0 = Empty
takeJDirect i s@(Single _ _)
  | i >= 1    = s
  | otherwise = Empty
takeJDirect i j@(Append _ j1 j2)
  | i >= size0 = j
  | i > size1  = let j2New   = takeJDirect (i - size1) j2
                     sizeNew = (sizedTag j1) <> (sizedTag j2New)
                 in Append sizeNew j1 j2New
  | otherwise  = takeJDirect i j1
  where
    size0 = getSizeFromTag j
    size1 = getSizeFromTag j1

takeJFold :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJFold = foldJoinList
    Empty
    (const Empty)
    (const Empty) id (const Empty)
    a0ge a1le a1le a1g
  where
    a0ge _     _ j                = j
    a1le _     i (Append _ j1 _) = takeJFold i j1
    a1le _     _ _               = Empty
    a1g  size1 i (Append _ j1 j2) =
      let j2New   = takeJFold (i - size1) j2
          sizeNew = (sizedTag j1) <> (sizedTag j2New)
      in Append sizeNew j1 j2New
    a1g  _     _ _                = Empty

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ = takeJFold


-- ex3
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

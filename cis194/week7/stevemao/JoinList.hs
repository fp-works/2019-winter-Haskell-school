{-# OPTIONS_GHC -Wall #-}
module JoinList where
import Sized

data JoinList m a = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
Empty +++ a = a
a +++ Empty = a
a +++ b = Append (tag a <> tag b) a b

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

getIntSize :: Sized a => a -> Int
getIntSize = getSize . size

instance Sized m => Sized (JoinList m a) where
  size Empty = Size 0
  size (Single m _) = size m
  size (Append m _ _) = size m

indexJ :: (Sized b, Monoid b) =>
  Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i (Single _ a)
  | i == 0 = Just a
  | otherwise = Nothing
indexJ i (Append _ a b)
  | i < sizeA = indexJ i a
  | otherwise = indexJ (i - sizeA) b
  where sizeA = getIntSize a

dropJ :: (Sized b, Monoid b) =>
  Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ i l@(Single _ _)
  | i < getIntSize l = l
  | otherwise = Empty
dropJ i (Append _ a b)
  | i < sizeA = dropJ i a +++ b
  | i > sizeA = dropJ (i - sizeA) b
  | otherwise = b
  where sizeA = getIntSize a

takeJ :: (Sized b, Monoid b) =>
  Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ i l@(Single _ _)
  | i >= getIntSize l = l
  | otherwise = Empty
takeJ i (Append _ a b)
  | i < sizeA = takeJ i a
  | i > sizeA = a +++ takeJ (i - sizeA) b
  | otherwise = a
  where sizeA = getIntSize a

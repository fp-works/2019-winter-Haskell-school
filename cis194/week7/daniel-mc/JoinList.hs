{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module JoinList
  ( JoinList(..)
  , tag
  , (+++)
  , indexJ
  , dropJ
  , takeJ
  )
where

import           Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

-- Exercise 1

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single x _  ) = x
tag (Append x _ _) = x

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x     Empty = x
(+++) Empty y     = y
(+++) x     y     = Append (tag x <> tag y) x y

-- Exercise 2

instance (Sized m, Monoid m) => Sized (JoinList m a) where
  size Empty          = mempty
  size (Single s _  ) = size s
  size (Append s _ _) = size s

indexJ :: (Sized m, Monoid m) => Int -> JoinList m a -> Maybe a
indexJ i (Append _ l r) | lSize <= i = indexJ (i - lSize) r
                        | otherwise  = indexJ i l
  where lSize = getSize . size $ l
indexJ 0 (Single _ a) = Just a
indexJ _ _            = Nothing

dropJ :: (Sized m, Monoid m) => Int -> JoinList m a -> JoinList m a
dropJ n (Append _ l r) | lSize <= n = dropJ (n - lSize) r
                       | otherwise  = dropJ n l +++ r
  where lSize = getSize . size $ l
dropJ n j | n <= 0    = j
          | otherwise = Empty

takeJ :: (Sized m, Monoid m) => Int -> JoinList m a -> JoinList m a
takeJ n (Append _ l r) | lSize <= n = l +++ takeJ (n - lSize) r
                       | otherwise  = takeJ n l
  where lSize = getSize . size $ l
takeJ n j | n > 0     = j
          | otherwise = Empty

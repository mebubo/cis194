module JoinList where

import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving (Show, Eq)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
a +++ b = Append (tag a `mappend` tag b) a b

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

indexJ :: (Sized b, Monoid b) =>
    Int -> JoinList b a -> Maybe a
indexJ n Empty = Nothing
indexJ n (Single _ x) | n == 0 = Just x
                      | otherwise = Nothing
indexJ n (Append m l r) | n < 0 || n >= size' = Nothing
                        | n < size' = indexJ n l
                        | otherwise = indexJ (n-sizeLeft) r
    where size' = getSize $ size m
          sizeLeft = getSize $ size $ tag l

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n Empty = Empty
dropJ n x | n <= 0 = x
dropJ 1 (Single _ _) = Empty
dropJ n (Append m l r) | n >= size' = Empty
                       | n >= sizeLeft = dropJ (n-sizeLeft) r
                       | otherwise = dropJ n l +++ r
    where size' = getSize $ size m
          sizeLeft = getSize $ size $ tag l

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n Empty = Empty
takeJ n _ | n <= 0 = Empty
takeJ _ x@(Single _ _) = x
takeJ n x@(Append m l r) | n >= size' = x
                         | n > sizeLeft = l +++ takeJ (n-sizeLeft) r
                         | otherwise = takeJ n l
    where size' = getSize $ size m
          sizeLeft = getSize $ size $ tag l


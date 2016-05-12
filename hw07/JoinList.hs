{-# LANGUAGE FlexibleInstances #-}

module JoinList where

import Sized
import Scrabble
import Buffer

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

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l r) = jlToList l ++ jlToList r

indexJ :: (Sized b, Monoid b) =>
    Int -> JoinList b a -> Maybe a
indexJ n Empty = Nothing
indexJ n (Single _ x) | n == 0 = Just x
                      | otherwise = Nothing
indexJ n (Append m l r) | n < 0 || n >= size' = Nothing
                        | n < sizeLeft = indexJ n l
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

scoreLine :: String -> JoinList Score String
scoreLine line = iter ws
    where ws = words line
          iter :: [String] -> JoinList Score String
          iter [] = Empty
          iter [x] = Single (scoreString x) x
          iter xs = iter (take half xs) +++ iter (drop half xs)
              where half = length xs `div` 2

instance Buffer (JoinList (Score, Size) String)
    where
        toString = unlines . jlToList
        fromString str = iter ls
            where ls = lines str
                  iter [] = Empty
                  iter [x] = Single (scoreString x, 1) x
                  iter xs = iter (take half xs) +++ iter (drop half xs)
                    where half = length xs `div` 2
        line = indexJ
        replaceLine n l j = takeJ n j +++ fromString l +++ dropJ (n+1) j
        numLines = getSize . snd . tag
        value = getScore . fst . tag

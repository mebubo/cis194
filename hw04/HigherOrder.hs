module HigherOrder where

import Data.List

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x-2) * fun1 xs
    | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/=1) . iterate f
    where f n = if even n then n `div` 2 else 3*n + 1

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree [] = Leaf
foldTree xs =
    Node height
         (foldTree (take middle xs))
         (xs !! middle)
         (foldTree (drop (middle + 1) xs))
    where
        len = length xs
        middle = len `div` 2
        height = floor (logBase 2 (fromIntegral len))

xor :: [Bool] -> Bool
xor = foldr xor' False
    where
        xor' :: Bool -> Bool -> Bool
        xor' a b = (a && not b) || (not a && b)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a -> (f a:)) []

-- myFoldl :: (a -> b -> a) -> a -> [b] -> a
-- myFoldl f base xs = foldr

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+1) . (*2)) included
    where excluded = filter (<=n) [i + j + 2 * i * j | j <- [1..n], i <- [1..j]]
          included = [1..n] \\ excluded

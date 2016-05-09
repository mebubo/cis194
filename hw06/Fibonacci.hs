{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = [fib n | n <- [0..]]

fibs2 :: [Integer]
fibs2 = iter 0 1
    where iter :: Integer -> Integer -> [Integer]
          iter a b = a : iter b (a+b)

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = iter 1
    where iter :: Integer -> Stream Integer
          iter n = Cons (biggestPowerOfTwoDivisor n) (iter (n+1))
          intLog :: Integer -> Integer
          intLog n = ceiling $ logBase 2 (fromIntegral n)
          biggestPowerOfTwoDivisor :: Integer -> Integer
          biggestPowerOfTwoDivisor n = last [m | m <- [0..(intLog n)]
                                               , n `mod` 2^m == 0]

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = Cons x (interleaveStreams ys xs)

ruler' :: Stream Integer
ruler' = iter 0
    where iter n = interleaveStreams (streamRepeat n) (iter (n+1))

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
    fromInteger n = Cons n (streamRepeat 0)
    negate = streamMap (0-)
    Cons x xs + Cons y ys = Cons (x+y) (xs + ys)
    Cons a0 a' * b@(Cons b0 b') = Cons (a0*b0) (streamMap (*a0) b' + a'*b)

instance Fractional (Stream Integer) where
    Cons a0 a' / Cons b0 b' = q
        where q = Cons (a0 `div` b0) (streamMap (`div` b0) (a' - q * b'))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

data Matrix = Matrix Integer Integer Integer Integer

instance Num Matrix where
    Matrix a00 a01 a10 a11 * Matrix b00 b01 b10 b11 = Matrix (a00*b00 + a01*b10)
                                                             (a00*b01 + a01*b11)
                                                             (a10*b00 + a11*b10)
                                                             (a10*b10 + a11*b11)

f :: Matrix
f = Matrix 1 1 1 0
fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = extract (f^(n-1))
    where extract (Matrix a _ _ _) = a

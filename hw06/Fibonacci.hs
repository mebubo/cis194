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

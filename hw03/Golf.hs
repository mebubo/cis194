module Golf where

skips :: [a] -> [[a]]
skips xs = [everyNth n n xs | n <- [1..length xs]]
    where
        everyNth :: Int -> Int -> [a] -> [a]
        everyNth _ _ [] = []
        everyNth 1 n (x:xs) = x : (everyNth n n xs)
        everyNth i n (x:xs) = everyNth (i-1) n xs

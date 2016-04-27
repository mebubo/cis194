module Golf where

skips :: [a] -> [[a]]
skips xs = [everyNth n n xs | n <- [1..length xs]]
    where
        everyNth :: Int -> Int -> [a] -> [a]
        everyNth _ _ [] = []
        everyNth 1 n (x:xs) = x : (everyNth n n xs)
        everyNth i n (x:xs) = everyNth (i-1) n xs

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:zs)
    | x <= y && y >= z = y : rest
    | otherwise = rest
    where rest = localMaxima (y:z:zs)
localMaxima _ = []

histogram :: [Integer] -> String
histogram xs =
    let
        countOccur n = fromIntegral . length . filter (==n)
        occurs = [countOccur n xs | n <- [0..9]]
        nums = foldr (++) [] [show n | n <- [0..9]]
        separator = take 10 $ repeat '='
        lines' occ = [line occ n | n <- [1..(maximum occ)]]
        line occ n = [if x >= n then '*' else ' ' | x <- occ]
    in
        unlines . reverse $ nums:separator:lines' occurs



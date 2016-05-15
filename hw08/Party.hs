module Party where

import Employee
import Data.Tree
import Data.List (sort)

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL (e:es) (f + empFun e)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL es1 f1) (GL es2 f2) = GL (es1 `mappend` es2) (f1 + f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun l1 l2 = if l1 > l2 then l1 else l2

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f Node {rootLabel = rl, subForest = sf} =
  f rl (map (treeFold f) sf)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss gls = (withBoss', withoutBoss')
  where
    (withBoss, withoutBoss) = unzip gls
    withoutBoss' = mconcat withBoss
    withBoss' = glCons boss (mconcat withoutBoss)

maxFun :: Tree Employee -> GuestList
maxFun tree = max with without
  where
    (with, without) = treeFold nextLevel tree

main :: IO ()
main = readFile "company.txt" >>= putStr . transfrom

transfrom :: String -> String
transfrom s = formatGuestList $ maxFun company
  where
    company :: Tree Employee
    company = read s
    formatGuestList :: GuestList -> String
    formatGuestList (GL es fun) = total ++ guests
      where
        total = "Total fun: " ++ show fun ++ "\n"
        guests = unlines $ sort $ map empName es

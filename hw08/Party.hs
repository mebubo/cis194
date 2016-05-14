module Party where

import Employee
import Data.Tree

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL (e:es) (f + empFun e)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL es1 f1) (GL es2 f2) = GL (es1 `mappend` es2) (f1 + f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun l1 l2 = if l1 > l2 then l1 else l2

treeFold :: (a -> [b] -> b) -> b -> Tree a -> b
treeFold f seed Node {rootLabel = rl, subForest = sf} =
  f rl (map (treeFold f seed) sf)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss gls = (withBoss', withoutBoss')
  where
    (withBoss, withoutBoss) = unzip gls
    withoutBoss' = maximum' withBoss
    withBoss' = glCons boss (maximum' withoutBoss)
    maximum' :: (Ord a, Monoid a) => [a] -> a
    maximum' [] = mempty
    maximum' xs = maximum xs

maxFun :: Tree Employee -> GuestList
maxFun tree = max with without
  where
    (with, without) = treeFold nextLevel mempty tree

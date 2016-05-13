module Party where

import Employee

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL (e:es) (f + empFun e)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL es1 f1) (GL es2 f2) = GL (es1 `mappend` es2) (f1 + f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun l1 l2 = if l1 > l2 then l1 else l2

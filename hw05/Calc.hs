module Calc where

import ExprT
import Parser

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

evalStr :: String -> Maybe Integer
evalStr s = case parseExp Lit Add Mul s of
    Nothing -> Nothing
    Just x -> Just (eval x)

evalStr' :: String -> Maybe Integer
evalStr' = fmap eval . parseExp Lit Add Mul

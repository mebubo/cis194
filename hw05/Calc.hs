{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where

import ExprT
import Parser
import qualified StackVM
import qualified Data.Map as M

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

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

reify :: ExprT -> ExprT
reify = id

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (>0)
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)

instance Expr MinMax where
    lit = MinMax
    add = max
    mul = min

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
    lit = Mod7
    add (Mod7 a) (Mod7 b) = Mod7 . (`mod` 7) $ a + b
    mul (Mod7 a) (Mod7 b) = Mod7 . (`mod` 7) $ a * b

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

instance Expr StackVM.Program where
    lit x = [ StackVM.PushI x ]
    add p1 p2 = p1 ++ p2 ++ [StackVM.Add]
    mul p1 p2 = p1 ++ p2 ++ [StackVM.Mul]

testProgram = testExp :: Maybe StackVM.Program

execProgram :: Maybe StackVM.Program -> Either String StackVM.StackVal
execProgram (Just x) = StackVM.stackVM x

compile :: String -> Maybe StackVM.Program
compile = parseExp lit add mul

class HasVars a where
    var :: String -> a

data VarExprT = VLit Integer
              | Var String
              | VAdd VarExprT VarExprT
              | VMul VarExprT VarExprT
    deriving (Show)

instance Expr VarExprT where
    lit = VLit
    add = VAdd
    mul = VMul

instance HasVars VarExprT where
    var = Var

instance Expr (M.Map String Integer -> Maybe Integer) where
   lit x _ = Just x
   add f1 f2 m = do r1 <- f1 m
                    r2 <- f2 m
                    return (r1 + r2)
   mul f1 f2 m = do r1 <- f1 m
                    r2 <- f2 m
                    return (r1 * r2)

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs

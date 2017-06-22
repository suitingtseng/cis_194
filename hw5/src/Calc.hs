module Calc (
  eval,
  evalStr,
  lit,
  add,
  mul,
  ) where

import ExprT
import Parser

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add a b) = (eval a) + (eval b)
eval (Mul a b) = (eval a) * (eval b)

evalStr :: String -> Maybe Integer
evalStr s = case (parseExp  Lit Add Mul s) of
                Nothing -> Nothing
                Just exp -> Just $ eval exp

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit n = Lit n
    add a b = Add a b
    mul a b = Mul a b

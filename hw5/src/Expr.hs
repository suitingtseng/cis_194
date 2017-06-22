module Expr (
  Expr,
  lit,
  add,
  mul,
  ) where

class Expr e where
  lit :: Integer -> e
  add :: e -> e -> e
  mul :: e -> e -> e

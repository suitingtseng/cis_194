module ExprT where
import Expr

data ExprT = Lit Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT
  deriving (Show, Eq)

instance Expr.Expr ExprT where
  lit = Lit
  add x y = Add x y
  mul x y = Mul x y

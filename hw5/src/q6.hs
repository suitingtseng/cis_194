{-# LANGUAGE FlexibleInstances #-}
module Q6 where
import qualified Expr
import qualified ExprT
import qualified Data.Map as M

class HasVars a where
  var :: String -> a

data VarExprT = Lit Integer
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
              | Var String
  deriving (Show, Eq)

instance Expr.Expr VarExprT where
  lit = Lit
  add x y = Add x y
  mul x y = Mul x y

instance HasVars VarExprT where
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var x = \m -> M.lookup x m

instance Expr.Expr (M.Map String Integer -> Maybe Integer) where
  lit x = \_ -> Just x
  add x y = \m -> fmap (+) (x m) <*> (y m)
  mul x y = \m -> fmap (*) (x m) <*> (y m)

import qualified Calc as Calc
import ExprT
import Expr
import Parser (parseExp)



putEvalExprT :: ExprT -> IO()
putEvalExprT e = do
    putStrLn $ (show e) ++ " == " ++ (show $ Calc.eval e)

putEvalStr :: String -> IO()
putEvalStr s = do
    putStrLn $ (show s) ++ " == " ++ (show $ Calc.evalStr s)

main = do
    putEvalExprT (Mul (Add (Lit 5) (Lit 10)) (Mul (Lit 2) (Lit 9)))
    putEvalStr "(2+3)*4"
    let e = ((Calc.mul
                (Calc.add (Calc.lit 5) (Calc.lit 10))
                (Calc.mul (Calc.lit 2) (Calc.lit 9))) :: ExprT)
    putStrLn $ (show $ e) ++ " == " ++ (show $ Calc.eval e)

    putEvalExprT $ ((mul (add (lit 5) (lit 10)) (mul (lit 2) (lit 9))) :: ExprT)
    putStrLn $ show testInteger
    putStrLn $ show testBool
    putStrLn $ show testMM
    putStrLn $ show testSat


instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (>0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = lit $ max x y
  mul (MinMax x) (MinMax y) = lit $ min x y

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
  lit x = Mod7 $ mod x 7
  add (Mod7 x) (Mod7 y) = lit (x+y)
  mul (Mod7 x) (Mod7 y) = lit (x*y)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

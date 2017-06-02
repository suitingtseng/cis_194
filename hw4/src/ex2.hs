module EX2 (
  foldTree,
  ) where

import qualified Data.List as L

putFun1 :: Fun1 -> Fun1 -> [Integer] -> IO()
putFun1 f1 f2 xs = do
    putStrLn $ "fun1  " ++ (show xs) ++ " == " ++ (show $ f1 xs)
    putStrLn $ "fun1' " ++ (show xs) ++ " == " ++ (show $ f2 xs)

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

-- TODO
-- foldTree :: [a] -> Tree a
-- foldTree

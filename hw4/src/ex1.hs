module EX1 (
  fun1,
  fun1',
  fun2,
  fun2',
  putFun1,
  putFun2,
  ) where

import qualified Data.List as L

type Fun1 = [Integer] -> Integer

putFun1 :: Fun1 -> Fun1 -> [Integer] -> IO()
putFun1 f1 f2 xs = do
    putStrLn $ "fun1  " ++ (show xs) ++ " == " ++ (show $ f1 xs)
    putStrLn $ "fun1' " ++ (show xs) ++ " == " ++ (show $ f2 xs)

type Fun2 = Integer -> Integer
putFun2 :: Fun2 -> Fun2 -> Integer -> IO()
putFun2 f1 f2 a = do
    putStrLn $ "fun2  " ++ (show a) ++ " == " ++ (show $ f1 a)
    putStrLn $ "fun2' " ++ (show a) ++ " == " ++ (show $ f2 a)


fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = (L.foldl' (*) 1) . (L.map (subtract 2)) . (filter even)


fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . chain

chain :: Integer -> [Integer]
chain 1 = []
chain n
    | even n = n:(chain $ n `div` 2)
    | otherwise = chain (3 * n + 1)

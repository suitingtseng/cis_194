module EX3 (
  xor,
  xor',
  putXor,
  map',
  putMap,
  ) where

import Data.List as L

type XorF = [Bool] -> Bool

putXor :: XorF -> XorF -> [Bool] -> IO()
putXor f1 f2 xs = do
    putStrLn $ "xor  " ++ (show xs) ++ " == " ++ (show $ f1 xs)
    putStrLn $ "xor' " ++ (show xs) ++ " == " ++ (show $ f2 xs)

xor :: [Bool] -> Bool
xor = odd . length . (filter (==True))

xor' :: [Bool] -> Bool
xor' = L.foldl (\a b -> not (a==b)) False

type MapF = (Integer -> Integer) -> [Integer] -> [Integer]

putMap :: MapF -> MapF -> [Integer] -> IO()
putMap f1 f2 xs = do
    putStrLn $ "map (+5) " ++ (show xs) ++ " == " ++ (show $ f1 (+5) xs)
    putStrLn $ "map'(+5) " ++ (show xs) ++ " == " ++ (show $ f2 (+5) xs)


map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\b xs -> (f b):xs) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)

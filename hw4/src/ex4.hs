module EX4 (
  sieveSundaram,
  putSieve,
  ) where

putSieve :: Integer -> IO()
putSieve x = do
    putStrLn $ "Primes smaller " ++ (show (x*2+1)) ++ " : " ++ (show $ sieveSundaram x)

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = let
    l1 = [magicNum i j | i <- [1..n], j <- [1..n], (magicNum i j) <= n]
    l2 = filter (\x -> not (x `elem` l1)) [1..n]
    l3 = map (\x -> x * 2 + 1) l2
    in l3

magicNum :: Integer -> Integer -> Integer
magicNum x y = x + y + 2 * x * y

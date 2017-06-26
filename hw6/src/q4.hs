import Fibonacci

main = do
  putStrLn . show $ streamMap (*5) fibs2'
  putStrLn . show $ streamRepeat 100
  putStrLn . show $ streamFromSeed (*2) 1

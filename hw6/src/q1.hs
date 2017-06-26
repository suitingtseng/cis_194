import Fibonacci

main = do
  mapM_ (putStrLn . show . fib) [0..10]
  putStrLn . show $ take 15 fibs1

import AParser

main = do
  putStrLn . show $ runParser abParser "abc"
  putStrLn . show $ runParser abParser "bca"

  putStrLn . show $ runParser abParser_ "abc"
  putStrLn . show $ runParser abParser_ "bca"

  putStrLn . show $ runParser intPair "12 34"
  putStrLn . show $ runParser intPair "12 34bs"

  putStrLn . show $ runParser intOrUppercase "342abcd"
  putStrLn . show $ runParser intOrUppercase "XYZ"
  putStrLn . show $ runParser intOrUppercase "foo"

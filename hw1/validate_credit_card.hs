toDigits :: Integer -> [Integer]
toDigits a
    | a <= 0 = []
    | a < 10 = [a]
    | otherwise = (toDigits . toInteger $ (div a 10)) ++ [mod a 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev a
    | a <= 0 = []
    | a < 10 = [a]
    | otherwise = (mod a 10):(toDigits . toInteger $ (div a 10))

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleOdd . reverse

doubleEven :: [Integer] -> [Integer]
doubleEven [] = []
doubleEven (x:xs) = (x*2):(doubleOdd xs)

doubleOdd :: [Integer] -> [Integer]
doubleOdd [] = []
doubleOdd (x:xs) = (x):(doubleEven xs)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sumDigitsOfInteger x + sumDigits xs

sumDigitsOfInteger :: Integer -> Integer
sumDigitsOfInteger a
    | a < 10 = a
    | otherwise = (mod a 10) + (sumDigitsOfInteger . toInteger $ (div a 10))

mod10 :: Integer -> Integer
mod10 x = mod x 10

eq0 :: Integer -> Bool
eq0 x = x == 0

validate :: Integer -> Bool
validate = eq0 . mod10 . sumDigits . doubleEveryOther . toDigits

printValidate :: Integer -> IO ()
printValidate x = do
    putStrLn ("validate " ++ show x ++ " = " ++ (show . validate $ x))

main = do
    printValidate 4012888888881881
    printValidate 4012888888881882

import qualified Golf as G



putSkips :: (Show a) => [a] -> IO ()
putSkips xs = do
    putStrLn $ "skips " ++ (show xs) ++ " == " ++ (show $ G.skips xs)

putLocalMaxima :: [Integer] -> IO ()
putLocalMaxima xs = do
    putStrLn $ "localMaxima " ++ (show xs) ++ " == " ++ (show $ G.localMaxima xs)

putHistogram :: [Integer] -> IO()
putHistogram xs = do
    putStrLn $ "histogram of " ++ (show xs)
    putStrLn $ G.histogram xs


main = do
    putSkips "ABCD"
    putSkips "hello!"
    putSkips [1]
    putSkips [True, False]
    putSkips ([]::[Int])

    putLocalMaxima [2,9,5,6,1]
    putLocalMaxima [2,3,4,1,5]
    putLocalMaxima [1,2,3,4,5]

    putHistogram [1,1,1,5]
    putHistogram [1,4,5,4,6,6,3,4,2,4,9]

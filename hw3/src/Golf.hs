module Golf (
  skips,
  localMaxima,
  histogram,
  ) where
import Data.List as L
import Data.Map.Strict as M
{-
skips "ABCD" == ["ABCD", "BD", "C", "D"]
skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
skips [1] == [[1]]
skips [True,False] == [[True,False], [False]]
skips [] == []
-}
skips :: [a] -> [[a]]
skips xs = L.map (everyOther xs) [1..(length xs)]

everyOther :: [a] -> Int -> [a]
everyOther xs n = snd $ unzip $ L.filter (`modEq0` n) $ zip [1..] xs

modEq0 :: (Int, a) -> Int -> Bool
modEq0 (a, _) b = (mod a b) == 0


{-
localMaxima [2,9,5,6,1] == [9,6]
localMaxima [2,3,4,1,5] == [4]
localMaxima [1,2,3,4,5] == []
-}
localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima (x:[]) = []
localMaxima (x:y:[]) = []
localMaxima (x:y:z:xs)
    | y > (max x z) = y:(localMaxima (z:xs))
    | otherwise = localMaxima (y:z:xs)


{-
histogram [1,1,1,5] ==
*
*
* *
==========
0123456789
histogram [1,4,5,4,6,6,3,4,2,4,9] ==
*
*
* *
****** *
==========
0123456789
-}
histogram :: [Integer] -> String
histogram xs = let
    highToLow = sortedCounterList xs
    maxCount = snd $ head highToLow
    in unlines $ L.transpose $ L.map reverse $ L.map snd $ L.sortOn fst (L.map (generateLine maxCount) highToLow)

generateLine :: Integer -> (Integer, Integer) -> (Integer, String)
generateLine maxCount (index, count) = (index,
                                    (show index)
                                    ++ "="
                                    ++ (replicate (fromInteger count) '*')
                                    ++ (replicate (fromInteger (maxCount - count)) ' '))

sortedCounterList :: [Integer] -> [(Integer, Integer)]
sortedCounterList xs = sortOn (\(_, a) -> -a) (M.toList $ counterMapFromGroup $ group $ sort xs)

counterMapFromGroup :: [[Integer]] -> M.Map Integer Integer
counterMapFromGroup = L.foldr (\xs acc -> M.insert (head xs) (lengthInteger xs) acc)
                              (M.fromList (zip [0..9] (repeat 0)))

lengthInteger :: [a] -> Integer
lengthInteger xs = (toInteger (length xs))

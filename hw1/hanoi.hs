import qualified System.Environment as Sys (getArgs)

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi x p1 p2 p3
    | x == 0 = []
    | x == 1 = [(p1, p2)]
    | otherwise = (hanoi (x-1) p1 p3 p2) ++ [(p1, p2)] ++ (hanoi (x-1) p3 p2 p1)

main = do
    (x:_) <- Sys.getArgs
    putStrLn (show (hanoi (read x :: Integer) "a" "b" "c"))

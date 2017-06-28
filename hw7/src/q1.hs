import JoinList

main = do
    let jl1 = Single [1, 2] "a"
        jl2 = Single [3, 4] "b"
    putStrLn . show $ jl1 +++ jl2

    let jl3 = Empty
        jl4 = Single [3, 4] "b"
    putStrLn . show $ jl3 +++ jl4

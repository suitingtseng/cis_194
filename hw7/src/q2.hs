import JoinList
import Sized

main = do
  let jl = Append (Size 5)
             (Append (Size 3)
               (Single (Size 1) "c")
               (Append (Size 2)
                 (Single (Size 1) "d")
                 (Single (Size 1) "e")))
             (Append (Size 2)
               (Single (Size 1) "a")
               (Single (Size 1) "b"))
  mapM_ (putStrLn . show . (\i -> indexJ i jl)) [-3..8]
  mapM_ (putStrLn . show . (\i -> dropJ i jl)) [-3..8]
  mapM_ (putStrLn . show . (\i -> takeJ i jl)) [-3..8]

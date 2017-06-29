module Party where
import Employee
import Data.Tree

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es fun) = GL (e:es) (fun+(empFun e))

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL xs x_fun) (GL ys y_fun) = GL (xs++ys) (x_fun+y_fun)

moreFun :: GuestList -> GuestList -> GuestList
moreFun x y
  | x < y     = y
  | otherwise = x

treeFold :: b -> (a -> [b] -> b) -> Tree a -> b
treeFold acc f (Node {rootLabel = label, subForest = []}) = f label [acc]
treeFold acc f (Node {rootLabel = label, subForest = ns}) =
  f label (map (treeFold acc f) ns)

-- combineGLs :: Employee -> [GuestList] -> GuestList

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss dogs = (funWBoss, funWoBoss)
                    where funWBoss = mconcat $ bossGL:(map snd dogs)
                          funWoBoss = mconcat $ map (moreFun <$> fst <*> snd) dogs
                          bossGL = GL [boss] (empFun boss)

maxFun :: Tree Employee -> GuestList
maxFun tree = moreFun <$> fst <*> snd $ treeFold (mempty, mempty) nextLevel tree

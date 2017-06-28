module JoinList where
import Sized
import Scrabble

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x y = Append (mappend (tag x) (tag y)) x y

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single x _) = x
tag (Append x _ _) = x

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty        = Nothing
indexJ 0 (Single _ x) = Just x
indexJ _ (Single _ _) = Nothing
indexJ x (Append a left right)
  | outOfRange x      = Nothing
  | inLeft x          = indexJ x left
  | otherwise         = indexJ (x - left_size) right
  where all_size      = getSize . size $ a
        left_size     = getSize . size . tag $ left
        outOfRange    = (&&) <$> (<0) <*> (>=all_size)
        inLeft        = (>=) (left_size - 1)

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty         = Empty
dropJ x all@(Single _ _)
  | x >= 1            = Empty
  | otherwise         = all
dropJ x all@(Append a left right)
  | x >= all_size     = Empty
  | x < 0             = all
  | inLeft x          = (dropJ x left) +++ right
  | otherwise         = dropJ (x - left_size) right
  where all_size      = getSize . size $ a
        left_size     = getSize . size . tag $ left
        inLeft        = (>=) (left_size - 1)

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty         = Empty
takeJ x all@(Single _ _)
  | x >= 1            = all
  | otherwise         = Empty
takeJ x all@(Append a left right)
  | x >= all_size     = all
  | x < 0 = Empty
  | inLeft x          = takeJ x left
  | otherwise         = left +++ (takeJ (x - left_size) right)
  where all_size      = getSize . size $ a
        left_size     = getSize . size . tag $ left
        inLeft        = (>=) (left_size - 1)

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

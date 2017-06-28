{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where
import Data.Char (toUpper, isLetter)

newtype Score = Score Int deriving (Eq, Ord, Show, Num)

instance Monoid Score where
  mempty = Score 0
  mappend = (+)

score :: Char -> Score
score t
  | x `elem` "BCMP"  = Score 3
  | x `elem` "DG"    = Score 2
  | x `elem` "FVWYH" = Score 4
  | x `elem` "JX"    = Score 8
  | x `elem` "QZ"    = Score 10
  | x `elem` "K"     = Score 5
  | isLetter x       = Score 1
  | otherwise        = Score 0
  where x = toUpper t


scoreString :: String -> Score
scoreString = foldl mappend mempty . (map score)

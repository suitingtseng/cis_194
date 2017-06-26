{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = (fib $ n-1) + (fib $ n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = 0:1:(fibStream 0 1)

fibStream :: Integer -> Integer -> [Integer]
fibStream x y = let
                z = x + y
                nextStream = fibStream y z
                in z:nextStream

data Stream a = Cons' a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons' x xs) = x:(streamToList xs)

instance Show a => Show (Stream a) where
  show x = show . (take 20) . streamToList $ x

fibs2' :: Stream Integer
fibs2' = 0 `Cons'` (1 `Cons'` (fibStream' 0 1))

fibStream' :: Integer -> Integer -> Stream Integer
fibStream' x y = let
                z = x + y
                nextStream' = fibStream' y z
                in z `Cons'` nextStream'

streamRepeat :: a -> Stream a
streamRepeat x = x `Cons'` (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons' x xs) = (f x) `Cons'` (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = x `Cons'` (streamFromSeed f $ f x)

nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) (streamMap (+1) ruler)

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons' x xs) ys = x `Cons'` (interleaveStreams ys xs)

x :: Stream Integer
x = 0 `Cons'` (1 `Cons'` streamRepeat 0)

instance Num (Stream Integer) where
  fromInteger x = Cons' x (streamRepeat 0)
  negate (Cons' x xs) = Cons' (-x) (negate xs)
  (Cons' x xs) + (Cons' y ys) = Cons' (x+y) (xs+ys)
  a@(Cons' a0 a') * b@(Cons' b0 b') = Cons' (a0*b0) ((streamMap (*a0) b') + (a' * b))

instance Fractional (Stream Integer) where
  -- fromRational x = Cons' (toInteger x) (streamRepeat 0)
  a@(Cons' a0 a') / b@(Cons' b0 b') = let
                                      c0 = div a0 b0
                                      c' = (a' - (a / b * b'))
                                      in Cons' c0 (streamMap (\x -> div x b0) c')

fibs3 :: Stream Integer
fibs3 = let
        a = 0 `Cons'` (1 `Cons'` streamRepeat 0)
        b = 1 `Cons'` (-1 `Cons'` (1 `Cons'` streamRepeat 0))
        in a/b

data Matrix = Matrix Integer Integer Integer Integer

instance Num Matrix where
  (Matrix a1 b1 c1 d1) * (Matrix a2 b2 c2 d2) =
    let a3 = a1*a2 + b1*c2
        b3 = a1*b2 + b1*d2
        c3 = c1*a2 + d1*c2
        d3 = c1*b2 + d1*d2
    in Matrix a3 b3 c3 d3

f = Matrix 1 1 1 0
fib4 :: Integer -> Integer
fib4 x =
  let Matrix _ y _ _ = f^x
  in y

{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

nums :: [Integer]
nums = [0..]

fibs1 :: [Integer]
fibs1 = map fib nums

fibs2 :: [Integer]
fibs2 = maker 0 1 
  where
    maker x y = x : maker y (x+y)

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons v stream) = v : streamToList stream

instance Show a => Show (Stream a) where
  show stream = unwords $ map show (take 20 (streamToList stream))

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x strm) = Cons (f x) (streamMap f strm)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- the ruler function

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger val = Cons val (streamRepeat 0)
  negate (Cons val strm) = Cons (negate val) (negate strm)
  (+) (Cons val1 strm1) (Cons val2 strm2) = Cons (val1 + val2) (strm1 + strm2)
  (*) (Cons a strmA) (Cons b strmB) = Cons (a * b) (strmA * Cons b strmB + streamMap (* a) strmB)

instance Fractional (Stream Integer) where
  (/) (Cons v1 s1) (Cons v2 s2) = q 
    where 
      constVal = v1 `div` v2
      q = Cons constVal (streamMap (\x -> x `div` v2) (s1 - q * s2))

fibs3 :: Stream Integer 
fibs3 = x / (1 - x - x^2)


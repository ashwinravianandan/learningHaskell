module Fibonacci where

fibonacci :: Integer -> Integer
fibonacci x 
  | 0 == x = 1
  | 1 == x = 1
  | otherwise = fibonacci (x-1) + fibonacci(x-2)
 
find :: [Integer] -> Integer -> Bool
find [] _ = False
find  (f : fs ) x = case compare f x of
  GT -> False
  LT -> find fs x
  EQ -> True

findInFib :: Integer -> Bool
findInFib = find $ map fibonacci [0..]


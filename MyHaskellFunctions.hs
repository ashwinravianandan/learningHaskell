module MyHaskellFunctions where

mysum ::( Num a ) => a -> a -> a
mysum a b = a+b

fact:: (Num a, Eq a) => a -> a
fact x
  | x == 1 = 1
  | x == 0 = 1
  | otherwise = x * fact ( x-1 )

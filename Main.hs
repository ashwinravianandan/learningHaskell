module Main where

fibonaci :: Integer -> Integer
fibonaci x 
  | 0 == x = 1
  | 1 == x = 1
  | otherwise = fibonaci (x-1) + fibonaci(x-2)
 
find :: Integer -> [Integer]-> String
find _ [] = "Not Found"
find  x (f : fs )
   | x > f = find x fs
   | x == f = "Found it"
   | otherwise = "Not a fib"

main :: IO()
main = do
  putStrLn "Enter number you want to test for fibonacci:"
  number <- getLine
  let 
    num :: Integer
    num = read number
  putStrLn $ find num $ map fibonaci [0..]


  -- warp, qfpl, applied-fp-course
  -- hayoo, local-hoogle

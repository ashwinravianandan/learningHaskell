module Main where
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

main :: IO()
main = do
  putStrLn "Enter number you want to test for fibonacci:"
  val <-  (findInFib . read) <$> getLine
  if True == val then
    putStrLn "The value occurs in the fibonacci series"
  else putStrLn "This is not a fibonacci number"

  -- warp, qfpl, applied-fp-course
  -- hayoo, local-hoogle

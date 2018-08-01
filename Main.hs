module Main where
import Data.Char
import Data.Maybe
import BST

values = [5,3,8,7,5,2,4,76,54,23,09,67]
mytree = foldl insert ( Leaf . head $ values ) ( tail values )

main :: IO()
main = print . tolist $ mytree

--  putStrLn "Enter number you want to test for fibonacci:"
--  val <-  (findInFib . read) <$> getLine
--  if val then
--    putStrLn "The value occurs in the fibonacci series"
--  else putStrLn "This is not a fibonacci number"
 -- warp, qfpl, applied-fp-course
  -- hayoo, local-hoogle

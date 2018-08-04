module Main where
import Data.Char
import Data.Maybe
import BST
import Heap

mytree = foldl insert ( Leaf . head $ values ) ( tail values )

values = [8,3,5,7,5,2,4,76,54,23,09,67]

heap = Heap values :: BinHeap Int

main :: IO()
main = print $ makeheap values

--  putStrLn "Enter number you want to test for fibonacci:"
--  val <-  (findInFib . read) <$> getLine
--  if val then
--    putStrLn "The value occurs in the fibonacci series"
--  else putStrLn "This is not a fibonacci number"
 -- warp, qfpl, applied-fp-course
  -- hayoo, local-hoogle

module Main where
import Data.Monoid
import SegmentTree

segtree = construct (map Sum [1,2,3,4,5,6,7,8])

main :: IO()
main = print $ value 7 segtree

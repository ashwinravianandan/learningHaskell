module BST where

data Tree a = Leaf a | Node (Maybe ( Tree a ) ) a (Maybe ( Tree a ))

insert :: Ord a => Tree a -> a -> Tree a
insert ( Leaf x ) a
   | a < x  = Node ( Just ( Leaf a ) ) x Nothing
   | otherwise = Node Nothing x ( Just ( Leaf a ) )

insert ( Node Nothing  x ( Just y ) ) a
   | a < x = Node ( Just ( Leaf a ) ) x ( Just y )
   | otherwise = Node Nothing  x ( Just ( insert y a) )

insert  ( Node ( Just v )  x ( Just y ) ) a
   | a < x = Node ( Just ( insert v a ) ) x ( Just y )
   | otherwise = Node ( Just v ) x ( Just ( insert y a ) )

insert  ( Node ( Just l )  x Nothing ) a
   | a < x = Node ( Just ( insert l a ) ) x Nothing
   | otherwise = Node ( Just l ) x ( Just ( Leaf a ) )


tolist :: Tree a -> [a]
tolist ( Leaf a ) = [a]
tolist ( Node ( Just l ) a ( Just r ) ) = tolist l ++ [a] ++ tolist r
tolist ( Node Nothing a ( Just r ) ) = a : tolist r
tolist ( Node ( Just l ) a Nothing ) = tolist l ++ [ a ]

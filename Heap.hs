module Heap where

data BinHeap a = Heap [a]
   deriving ( Show, Ord, Eq )

instance Functor BinHeap where
   -- fmap ( a->b ) -> BinHeap a -> BinHeap b
   fmap f ( Heap [a] ) = Heap( map f [a] )


children :: Ord a => BinHeap a -> Int -> [( a, Int )]
children (Heap []) _ =  []
children ( Heap [x]) _ = []
children ( Heap xs ) i
  | length xs < ( 2*i + 2 ) = []
  | length xs == ( 2*i + 2 ) = [( xs !! ( 2*i + 1 ), 2*i + 1 )]
  | otherwise = [ ( xs !! ( 2*i +1 ), 2*i + 1 ) , ( xs !! ( 2*i + 2 ), 2*i + 2 )]

swap :: Ord a => BinHeap a -> Int -> Int -> BinHeap a
swap ( Heap [] ) _ _ = Heap []
swap ( Heap [x] ) _ _ = Heap [x]
swap ( Heap [x, y] ) _ _ = Heap [y, x]
swap ( Heap xs ) i j 
  | i == j  = Heap xs
  | i > j = swap ( Heap xs ) j i
  | otherwise = Heap $ first_seg ++ [v1] ++ int_seg ++ [ v2 ] ++ last_seg
      where 
         v1 = xs !! j
         v2 = xs !! i
         first_seg = take i xs
         int_seg = take ( j-i-1 ) $ drop ( i+1 ) xs
         last_seg = drop ( j+1 ) xs

heapify :: Ord a => BinHeap a -> Int -> BinHeap a
heapify ( Heap[] ) _ = Heap []

heapify ( Heap [x,y] ) 0 
   | x < y = Heap [x,y]
   | otherwise = Heap [y, x]


heapify ( Heap ( a:b:c:xs ) ) 0
   | a > b = heapify ( swap ( Heap( a:b:c:xs) ) 0 1 ) 0
   | a > c = heapify ( swap ( Heap( a:b:c:xs) ) 0 2 ) 0
   | otherwise  = Heap ( a:b:c:xs )


heapify ( Heap xs ) i = case children ( Heap xs ) i of
                          [] -> heapify ( Heap xs ) ( ( i-1 ) `div` 2 )
                          [( u, j )] -> if ( xs !! i ) >= u then
                                       heapify ( swap ( Heap xs ) j i ) i
                                       else
                                       heapify ( Heap xs ) ( ( i-1 ) `div` 2 )
                          [( u, j ), ( v, k  )] 
                             | u < ( xs !! i ) ->  heapify ( swap ( Heap xs ) i j ) i
                             | v < ( xs !! i ) ->  heapify ( swap ( Heap xs ) i k ) i
                             | otherwise -> heapify ( Heap xs ) ( ( i-1 ) `div` 2 )

addtoheap :: Ord a => a  -> BinHeap a -> BinHeap a
addtoheap a ( Heap [] ) = Heap [a]
addtoheap a ( Heap xs ) = heapify ( Heap ( xs ++ [a] ) ) ( length xs )

makeheap :: Ord a => [a] -> BinHeap a
makeheap ( x:xs ) = foldr addtoheap ( Heap [x] ) xs


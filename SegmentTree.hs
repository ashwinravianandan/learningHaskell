module Main where
import Data.Monoid
import Data.Foldable

data SegmentTree a = Leaf a | PNode a (SegmentTree a) | Node a (SegmentTree a) (SegmentTree a)
   deriving (Show)

instance Functor SegmentTree where
   fmap f (Leaf a) = Leaf (f a)
   fmap f (Node a l r) = Node (f a) (fmap f l) (fmap f r)

instance Foldable SegmentTree where
   foldMap f (Leaf a) = f a
   foldMap f (Node a l r) = f a <> foldMap f l <> foldMap f r
   foldMap f (PNode a l) = f a <> foldMap f l

insert ::  (Monoid a, Eq a) =>  a -> SegmentTree a -> SegmentTree a
insert x (Leaf a) = Node (x <> a) (Leaf a) (Leaf x)
insert x ( PNode a (Leaf l)) = Node (l <> x) (Leaf l) (Leaf x)
insert x (Node a (Leaf l) (Leaf r)) = Node (a <> x) (Node a (Leaf l) (Leaf r)) (PNode x (Leaf x))

insert x (Node a l r) 
  | length l == length r = Node (a<>x) ( Node a l r) (PNode x (Leaf x))
  | otherwise = Node (l''<> v) l (Node v lt rt)
               where
                  Node v lt rt = insert x r
                  l'' = case l of
                          Node l' _ _ -> l'

construct :: (Monoid a, Eq a) => [a] -> SegmentTree a
construct xs = foldr insert (Leaf . head $ xs) (reverse . tail $ xs)

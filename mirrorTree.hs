data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

mirrorTree :: Tree a -> Tree a
mirrorTree Empty = Empty
mirrorTree (Node a x y) = Node a (mirrorTree y) (mirrorTree x)
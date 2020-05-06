data Tree a = Node a [Tree a]

treeDepth :: Tree a -> Int
treeDepth (Node a []) = 1
treeDepth (Node a list) = 1 + maximum (map treeDepth list)
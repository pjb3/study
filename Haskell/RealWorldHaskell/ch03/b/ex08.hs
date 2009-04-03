data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)
              
simpleTree = Node "parent" (Node "left child" Empty Empty)
                           (Node "right child" Empty Empty)

treeDepth :: Tree a -> Int
treeDepth Empty = 0
treeDepth (Node _ l r) = max (1 + (treeDepth l)) (1 + (treeDepth r))
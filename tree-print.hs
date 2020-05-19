data BinaryTree a = Leaf a | Node a (BinaryTree a) (BinaryTree a) deriving (Show)

t = Node 0 (Leaf 1) (Leaf 2)
t2 = Node 'a' (Leaf 'b') (Leaf 'c')
t3 = Node 'a' (Node 'b' (Leaf 'd') (Leaf 'e')) (Node 'c' (Leaf 'f') (Leaf 'g'))

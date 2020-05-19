data BinaryTree a = Nil | Leaf a | Node a (BinaryTree a) (BinaryTree a) deriving (Show)

t = Node 0 (Leaf 1) (Leaf 2)
t2 = Node 'a' (Leaf 'b') (Leaf 'c')
t3 = Node 'a' (Node 'b' (Leaf 'd') (Leaf 'e')) (Node 'c' (Leaf 'f') (Leaf 'g'))
t4 = Node 'a' (Node 'b' Nil (Leaf 'e')) (Node 'c' (Leaf 'f') (Leaf 'g'))
t5 = Node 'a' (Node 'b' Nil (Leaf 'e')) (Node 'c' (Node 'z' (Leaf 'x') Nil) (Leaf 'g'))

spaces 0 a = show a ++ "\n"
spaces n a = ' ' : ' ' : spaces (n - 1) a

toStringH Nil = "Empty Tree\n"
toStringH bt = ts 0 bt
  where
    ts l (Leaf a) = spaces l a
    ts l Nil = ""
    ts l (Node a left right) = (ts (l + 1) right) ++ (spaces l a) ++ (ts (l + 1) left)

printTreeHorizontal t = do
  putStr (toStringH t)

height Nil = 0
height (Leaf _) = 0
height (Node _ left right) =
  let
    hl = height left
    hr = height right
  in
    (if hl > hr then hl else hr) + 1

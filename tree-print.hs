data BinaryTree a = Nil | Node a (BinaryTree a) (BinaryTree a) deriving (Show)

t = Node 0 (Node 1 Nil Nil) (Node 2 Nil Nil)
t2 = Node 1 (Node 2 (Node 4 Nil Nil) (Node 5 Nil Nil)) (Node 3 (Node 6 Nil Nil) (Node 7 Nil Nil))
t3 = Node 1 (Node 2 Nil (Node 5 Nil Nil)) (Node 3 (Node 6 Nil Nil) (Node 7 Nil Nil))
t4 = Node 'a' (Node 'b' (Node 'd' Nil Nil) Nil) (Node 'c' (Node 'e' Nil Nil) Nil)
t5 = Node 1 (Node 2 (Node 3 (Node 4 Nil Nil) Nil) Nil) Nil
t6 = Node 1 (Node 2 (Node 4 (Node 8 Nil Nil) (Node 9 Nil Nil)) (Node 5 (Node 10 Nil Nil) (Node 111 Nil Nil))) (Node 3 (Node 6 (Node 12 Nil Nil) (Node 13 Nil Nil)) (Node 7 (Node 14 Nil Nil) (Node 15 Nil Nil)))
t7 = Node 1 Nil (Node 2 Nil (Node 3 Nil (Node 4 Nil Nil)))
t8 = Node 'a' (Node 'b' (Node 'd' (Node 'h' Nil Nil) (Nil)) (Node 'e' (Nil) (Node 'k' Nil Nil))) (Node 'c' (Node 'f' (Node 'l' Nil Nil) (Node 'm' Nil Nil)) (Node 'g' (Node 'n' Nil Nil) (Node 'o' Nil Nil)))

spaces 0 = ""
spaces n = ' ' : spaces (n - 1)

repeat' s 0 = ""
repeat' s n = s ++ (repeat' s (n - 1))

interSpaces height level unit = 
  if level == height then
    unit
  else 
    2 * (interSpaces height (level + 1) unit) + 1

leftPadding height level unit
  | height == level = 0
  | otherwise = (2^(height - level - 1)) * (unit + 1) - (div unit 2) - 1

treeInfo Nil = (0, 1)
treeInfo (Node a left right) =
  let
    size = length (show a)
    (hl, sl) = treeInfo left
    (hr, sr) = treeInfo right
  in
    ((maximum [hl, hr]) + 1, (maximum [size, sl, sr]))

levelToString height unit level tree = helper level tree
  where
    padding s = drop s (spaces (interSpaces height level unit))
    helper 1 Nil = ' ' : (padding 0)
    helper i Nil = repeat' (' ' : (padding 0)) (2^(level - i))
    helper 1 (Node a left right) =
      let
        stringA = show a
      in
        stringA ++ (padding ((length stringA) - 1))
    helper i (Node a left right) =
      helper (i - 1) left ++ helper (i - 1) right

printLevels height maxSize tree = do levelsPrintLoop 1 tree
  where
    unit = if maxSize `mod` 2 == 0 then maxSize + 1 else maxSize
    levelsPrintLoop i tree = do
      putStr (drop 0 (spaces (leftPadding height i unit)))
      putStr (levelToString height unit i tree)
      if i == height then do
        putStr "\n"
      else do
        putStr "\n"
        levelsPrintLoop (succ i) tree

printTree tree = do
  let (h, s) = treeInfo tree
  printLevels h s tree



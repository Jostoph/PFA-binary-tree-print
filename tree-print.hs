{-
Binary Tree pretty printing
authors:
  Tiago Povoa
  Christoph Rouff soit Rueff
-}

-- Binary Tree structure
data BinaryTree a = Nil | Node a (BinaryTree a) (BinaryTree a) deriving (Show)

-- Trees for easy testing
t = Node 0 (Node 1 Nil Nil) (Node 2 Nil Nil)
t2 = Node 1 (Node 2 (Node 4 Nil Nil) (Node 5 Nil Nil)) (Node 3 (Node 6 Nil Nil) (Node 7 Nil Nil))
t3 = Node 1 (Node 2 Nil (Node 5 Nil Nil)) (Node 3 (Node 6 Nil Nil) (Node 7 Nil Nil))
t4 = Node 'a' (Node 'b' (Node 'd' Nil Nil) Nil) (Node 'c' (Node 'e' Nil Nil) Nil)
t5 = Node 1 (Node 2 (Node 3 (Node 4 Nil Nil) Nil) Nil) Nil
t6 = Node 1 (Node 2 (Node 4 (Node 8 Nil Nil) (Node 9 Nil Nil)) (Node 5 (Node 10 Nil Nil) (Node 111 Nil Nil))) (Node 3 (Node 6 (Node 12 Nil Nil) (Node 13 Nil Nil)) (Node 7 (Node 14 Nil Nil) (Node 15 Nil Nil)))
t7 = Node 1 Nil (Node 2 Nil (Node 3 Nil (Node 4 Nil Nil)))
t8 = Node 'a' (Node 'b' (Node 'd' (Node 'h' Nil Nil) (Nil)) (Node 'e' (Nil) (Node 'k' Nil Nil))) (Node 'c' (Node 'f' (Node 'l' Nil Nil) (Node 'm' Nil Nil)) (Node 'g' (Node 'n' Nil Nil) (Node 'o' Nil Nil)))
t9 = Node "Albert" (Node "Bruce" (Node "David" Nil Nil) (Node "Eric" Nil Nil)) (Node "Charlie" (Node "Fabien" Nil Nil) (Node "Greg" Nil Nil))
t10 = Node [1,2,3] (Node [1,2,3,4] Nil Nil) (Node [2,3,5,2] Nil Nil)
t11 = Node 1 (Node 2 (Node 44 Nil Nil) (Node 5 Nil Nil)) (Node 3 (Node 6 Nil Nil) (Node 7 Nil Nil))6

-- Generate a String of n spaces
spaces 0 = ""
spaces n = ' ' : spaces (n - 1)

-- Repeat a String n times
repeat' s 0 = ""
repeat' s n = s ++ (repeat' s (n - 1))

{-
Computes the maximal space to add after an element,
given the tree-height, the element's level and the minimal space unit
-}
interSpaces height level unit = 
  if level == height then
    unit
  else 
    2 * (interSpaces height (level + 1) unit) + 1

{-
Compute the space padding to add in front of every line,
given the tree-height, the level and the minimal space unit
-}
leftPadding height level unit
  | height == level = 0
  | otherwise = (2^(height - level - 1)) * (unit + 1) - (div unit 2) - 1

{-
Compute the tree informnation needed before the printing.
The height and the maximum element size (as string).
-}
treeInfo Nil = (0, 1)
treeInfo (Node a left right) =
  let
    size = length (show a)
    (hl, sl) = treeInfo left
    (hr, sr) = treeInfo right
  in
    ((maximum [hl, hr]) + 1, (maximum [size, sl, sr]))

{-
Returns a given level as a string by computing addapted spaces offsets
for the elements.
If some elements are missing due to an empty parent (Nil at a higher level), it computes
the right amount of spaces this missing parent causes at this level.
Ex : If there is a Nil at level 2 (element missing) then there will be 4 missing elements at level 4,
so we need to add spaces for 4 elements.
-}
levelToString height unit level tree = helper level tree
  where
    padding s = drop s (spaces (interSpaces height level unit))
    helper 1 Nil = ' ' : (padding 0)                            -- missing element on given level
    helper i Nil = repeat' (' ' : (padding 0)) (2^(level - i))  -- missing element on a higher level
    helper 1 (Node a left right) =
      let
        stringA = show a
      in
        stringA ++ (padding ((length stringA) - 1))
    helper i (Node a left right) =
      helper (i - 1) left ++ helper (i - 1) right

{-
Print every level of the tree (line by line) using an inner "loop function".
The minimal elements inter-spacing (unit) is computed given the size of the largest
element in the tree and is addapted to be odd.
-}
printLevels height maxSize tree = do levelsPrintLoop 1 tree
  where
    unit = if maxSize `mod` 2 == 0 then maxSize + 1 else maxSize
    levelsPrintLoop i tree = do
      putStr (spaces (leftPadding height i unit))     -- print left padding
      putStr (levelToString height unit i tree)       -- print line
      if i == height then do
        putStr "\n"
      else do
        putStr "\n"
        levelsPrintLoop (succ i) tree

{-
Main function to pretty print a binary tree.
It computes the necessary information (height and max element size) and
calls the printing loop.
-}
printTree tree = do
  let (h, s) = treeInfo tree
  printLevels h s tree



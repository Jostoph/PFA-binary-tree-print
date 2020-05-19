data BinaryTree a = Nil | Node a (BinaryTree a) (BinaryTree a) deriving (Show)

t = Node 0 (Node 1 Nil Nil) (Node 2 Nil Nil)
t3 = Node 1 (Node 2 (Node 4 Nil Nil) (Node 5 Nil Nil)) (Node 3 (Node 6 Nil Nil) (Node 7 Nil Nil))
t4 = Node 1 (Node 2 Nil (Node 5 Nil Nil)) (Node 3 (Node 6 Nil Nil) (Node 7 Nil Nil))
-- t5 = Node 'a' (Node 'b' (Node 'd' Nil Nil) Nil) (Node 'c' (Node 'f' Nil Nil) Nil)
-- t6 = Node 'a' (Node 'b' (Node 'c' (Node 'd' Nil Nil) Nil) Nil) Nil
-- t7 = Node 'a' (Node 'b' (Node 'd' (Node '1' Nil Nil) (Node '2' Nil Nil)) (Node 'e' (Node '3' Nil Nil) (Node '4' Nil Nil))) (Node 'c' (Node 'f' (Node '5' Nil Nil) (Node '6' Nil Nil)) (Node 'g' (Node '7' Nil Nil) (Node '8' Nil Nil)))
-- udc = Node 'a' Nil (Node 'b' Nil (Node 'c' Nil (Node 'd' Nil Nil)))

spaces 0 = ""
spaces n = ' ' : spaces (n - 1)

repeat' s 0 = ""
repeat' s n = s ++ (repeat' s (n - 1))

interSpaces h l = 
  if l == h then
    1
  else 
    2 * (interSpaces h (l + 1)) + 1

height Nil = 0
height (Node _ left right) =
  let
    hl = height left
    hr = height right
  in
    (if hl > hr then hl else hr) + 1

levelToString height level tree = helper height level level tree
  where
    helper h l 1 Nil = '.' : spaces (interSpaces h l)
    helper h l i Nil = repeat' ('.' : spaces (interSpaces h l)) (2**(l - i))
    helper h l 1 (Node a left right) = (show a) ++ spaces (interSpaces h l)
    helper h l i (Node a left right) =
      helper h l (i - 1) left ++ helper h l (i - 1) right

printLevels height tree = do levelsPrintLoop 1 height tree
  where
    levelsPrintLoop i height tree = do
      putStr (spaces (2**(height - i) - 1))
      putStr (levelToString height i tree)
      if i == height then do
        putStr "\n"
      else do
        putStr "\n"
        levelsPrintLoop (succ i) height tree

printTree tree = do
  let h = height tree
  printLevels h tree



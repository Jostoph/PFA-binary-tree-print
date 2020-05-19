data BinaryTree a = Nil | Node a (BinaryTree a) (BinaryTree a) deriving (Show)

t = Node 0 (Node 1 Nil Nil) (Node 2 Nil Nil)
t2 = Node 'a' (Node 'b' Nil Nil) (Node 'c' Nil Nil)
t3 = Node 'a' (Node 'b' (Node 'd' Nil Nil) (Node 'e' Nil Nil)) (Node 'c' (Node 'f' Nil Nil) (Node 'g' Nil Nil))
t4 = Node 'a' (Node 'b' Nil (Node 'e' Nil Nil)) (Node 'c' (Node 'f' Nil Nil) (Node 'g' Nil Nil))
t5 = Node 'a' (Node 'b' (Node 'd' Nil Nil) Nil) (Node 'c' (Node 'f' Nil Nil) Nil)
t6 = Node 'a' (Node 'b' (Node 'c' (Node 'd' Nil Nil) Nil) Nil) Nil
udc = Node 'a' Nil (Node 'b' Nil (Node 'c' Nil (Node 'd' Nil Nil)))

spaces 0 = ""
spaces n = ' ' : spaces (n - 1)

repeat' s 0 = ""
repeat' s n = s ++ (repeat' s (n - 1))

interSpaces h l = 2*(h - l) + 1

height Nil = 0
height (Node _ left right) =
  let
    hl = height left
    hr = height right
  in
    (if hl > hr then hl else hr) + 1

levelToString height level tree = helper height level level tree
  where
    helper h l 1 Nil = '-' : spaces (interSpaces h l)
    helper h l i Nil = repeat' ('-' : spaces (interSpaces h l)) (2**(l - i))
    helper h l 1 (Node a left right) = a : spaces (interSpaces h l)
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



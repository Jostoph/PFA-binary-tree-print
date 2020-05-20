# Prog. Fonctionnelle Avancée: Laboratoire 9

### PFA - Printing of a binary tree (Horizontal order)

> Christoph Rouff Soit Rueff, Tiago Povoa

## Contexte

> Problème de l'impression d'un arbre binaire : Si on l'imprime en le tournant de 90 degrés c'est facile, on imprime d'abord un sous-arbre, ensuite l'élément racine, ensuite l'autre sous-arbre. L'imprimer à la verticale c'est une autre histoire...

## Fichiers et Modules

* `tree-print.hs` : TODO

## Implémentation

### Implémentation de l'arbre binaire

Au départ, on avait imaginé notre arbre comme ceci

```haskell
data BinaryTree a = Leaf a | Node a (BinaryTree a) (BinaryTree a) deriving (Show)
```

Soit une feuille, soit un node. Par la suite, nous avons réalisé que ça nous forçait à faire davantage de tests de cas. Du coup, on a fait varier vers:

```haskell
data BinaryTree a = Nil | Node a (BinaryTree a) (BinaryTree a) deriving (Show)
```

Dans ce cas, une feuille est juste un Node qui a deux enfants Nil. 

#### Exemple d'arbres

```haskell
t3 = Node 1 (Node 2 (Node 4 Nil Nil) (Node 5 Nil Nil)) (Node 3 (Node 6 Nil Nil) (Node 7 Nil Nil))
t4 = Node 1 (Node 2 Nil (Node 5 Nil Nil)) (Node 3 (Node 6 Nil Nil) (Node 7 Nil Nil))
```

TODO: compléter

### Première approche

Avant de démarrer l'impression de l'arbre à la vertical, on a essayé de le faire à l'horizontale.Ainsi, on a pu se faire une idée du processus avec un problème beaucoup plus simple à résoudre.

```haskell
toStringH Nil = "Empty Tree\n"
toStringH bt = ts 0 bt
  where
    ts l (Leaf a) = spaces l a
    ts l Nil = ""
    ts l (Node a left right) = (ts (l + 1) right) ++ (spaces l a) ++ (ts (l + 1) left)

printTreeHorizontal t = do
  putStr (toStringH t)
```

Note: à cette étape, nous utilisions encore `leaf`. 

Le fait d'avoir fait cette approche avant l'autre nous a permis d'identifier certains besoins dans le calcul d'un arbre à la verticale.

### Impression de l'arbre vertical


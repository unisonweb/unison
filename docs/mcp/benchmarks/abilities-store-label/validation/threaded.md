``` unison
type Tree a = Leaf a | Branch (Tree a) (Tree a)

Tree.label : Tree a -> Tree (Nat, a)
Tree.label t =
  go : Nat -> Tree a -> (Nat, Tree (Nat, a))
  go n = cases
    Leaf a -> (n + 1, Leaf (n, a))
    Branch l r ->
      (n1, l') = go n l
      (n2, r') = go n1 r
      (n2, Branch l' r')
  at2 (go 0 t)

test> Tree.label.tests.example = test.verify do
  ensureEqual (Tree.label (Branch (Leaf "a") (Branch (Leaf "b") (Leaf "c")))) (Branch (Leaf (0, "a")) (Branch (Leaf (1, "b")) (Leaf (2, "c"))))
  ensureEqual (Tree.label (Leaf "x")) (Leaf (0, "x"))
```
``` ucm
scratch/main> add
scratch/main> test
```

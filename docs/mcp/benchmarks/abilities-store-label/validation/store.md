``` unison
type Tree a = Leaf a | Branch (Tree a) (Tree a)

Tree.label : Tree a -> Tree (Nat, a)
Tree.label t =
  go : Tree a ->{Store Nat} Tree (Nat, a)
  go = cases
    Leaf a ->
      n = Store.get
      Store.put (n + 1)
      Leaf (n, a)
    Branch l r ->
      l' = go l
      r' = go r
      Branch l' r'
  Store.withInitialValue 0 do go t

test> Tree.label.tests.example = test.verify do
  ensureEqual (Tree.label (Branch (Leaf "a") (Branch (Leaf "b") (Leaf "c")))) (Branch (Leaf (0, "a")) (Branch (Leaf (1, "b")) (Leaf (2, "c"))))
  ensureEqual (Tree.label (Leaf "x")) (Leaf (0, "x"))
  ensureEqual (Tree.label (Branch (Branch (Leaf "a") (Leaf "b")) (Branch (Leaf "c") (Leaf "d")))) (Branch (Branch (Leaf (0, "a")) (Leaf (1, "b"))) (Branch (Leaf (2, "c")) (Leaf (3, "d"))))
```
``` ucm
scratch/main> add
scratch/main> test
```

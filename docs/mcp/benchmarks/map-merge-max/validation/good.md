``` unison
Map.mergeMax : Map Text Nat -> Map Text Nat -> Map Text Nat
Map.mergeMax = Map.unionWith Nat.max

test> Map.mergeMax.tests.examples = test.verify do
  ensureEqual (Map.toList (Map.mergeMax (Map.fromList [("a", 3), ("b", 9)]) (Map.fromList [("b", 4), ("c", 1)]))) [("a", 3), ("b", 9), ("c", 1)]
  ensureEqual (Map.toList (Map.mergeMax (Map.fromList [("b", 4)]) (Map.fromList [("b", 9)]))) [("b", 9)]
  ensureEqual (Map.toList (Map.mergeMax Map.empty (Map.fromList [("x", 0)]))) [("x", 0)]
  ensureEqual (Map.toList (Map.mergeMax Map.empty (Map.empty : Map Text Nat))) []
```
``` ucm
scratch/main> add
scratch/main> test
```

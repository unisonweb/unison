``` unison
Map.addCounts : Map k Nat -> Map k Nat -> Map k Nat
Map.addCounts = Map.unionWith (Nat.+)

test> Map.addCounts.tests.example = test.verify do
  ensureEqual (Map.toList (Map.addCounts (Map.fromList [("a", 1), ("b", 2)]) (Map.fromList [("b", 5), ("c", 1)]))) [("a", 1), ("b", 7), ("c", 1)]
  ensureEqual (Map.toList (Map.addCounts (Map.fromList [(1, 4)]) Map.empty)) [(1, 4)]
  ensureEqual (Map.toList (Map.addCounts Map.empty (Map.fromList [(1, 4)]))) [(1, 4)]
  ensureEqual (Map.toList (Map.addCounts (Map.fromList [(1, 4)]) (Map.fromList [(1, 6)]))) [(1, 10)]
  ensureEqual (Map.size (Map.addCounts (Map.empty : Map Nat Nat) Map.empty)) 0
```
``` ucm
scratch/main> add
scratch/main> test
```

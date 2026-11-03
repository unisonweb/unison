``` unison
Map.invert : Map k v -> Map v [k]
Map.invert m = Map.fromListWith (old new -> old ++ new) (List.map (cases (k, v) -> (v, [k])) (Map.toList m))

test> Map.invert.tests.example = test.verify do
  ensureEqual (Map.toList (Map.invert (Map.fromList [(1, "a"), (2, "b"), (3, "a")]))) [("a", [1, 3]), ("b", [2])]
  ensureEqual (Map.toList (Map.invert (Map.fromList [(1, 0), (2, 0), (3, 0), (4, 0)]))) [(0, [1, 2, 3, 4])]
  ensureEqual (Map.toList (Map.invert (Map.empty : Map Nat Nat))) []
  m = Map.invert (Map.fromList (List.map (n -> (n, n / 10)) (List.range 0 100)))
  ensureEqual (Map.size m) 10
  ensureEqual (List.all (l -> List.size l == 10) (Map.values m)) true
```
``` ucm
scratch/main> add
scratch/main> test
```

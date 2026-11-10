``` unison
Map.invert : Map k v -> Map v [k]
Map.invert m = Map.fromListWith (new old -> old ++ new) (List.map (cases (k, v) -> (v, [k])) (Map.toList m))

test> Map.invert.tests.example = test.verify do
  ensureEqual (Map.toList (Map.invert (Map.fromList [(1, "b"), (2, "a")]))) [("a", [2]), ("b", [1])]
```
``` ucm
scratch/main> add
scratch/main> test
```

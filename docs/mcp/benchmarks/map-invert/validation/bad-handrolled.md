``` unison
Map.invert : Map k v -> Map v [k]
Map.invert m =
  go acc = cases
    [] -> acc
    (k, v) +: rest -> go (Map.insert v (Optional.getOrElse [] (Map.get v acc) :+ k) acc) rest
  go Map.empty (Map.toList m)

test> Map.invert.tests.example = test.verify do
  ensureEqual (Map.toList (Map.invert (Map.fromList [(1, "a"), (2, "b"), (3, "a")]))) [("a", [1, 3]), ("b", [2])]
```
``` ucm
scratch/main> add
scratch/main> test
```

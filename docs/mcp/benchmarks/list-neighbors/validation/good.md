``` unison
List.neighbors : [a] -> [(a, a)]
List.neighbors = List.slidingPairs

test> List.neighbors.tests.example = test.verify do
  ensureEqual (List.neighbors [1, 2, 3, 4]) [(1, 2), (2, 3), (3, 4)]
  ensureEqual (List.neighbors ["a", "b", "c"]) [("a", "b"), ("b", "c")]
  ensureEqual (List.neighbors [1]) []
  ensureEqual (List.neighbors ([] : [Nat])) []
  ensureEqual (List.size (List.neighbors (List.range 0 100))) 99
```
``` ucm
scratch/main> add
scratch/main> test
```

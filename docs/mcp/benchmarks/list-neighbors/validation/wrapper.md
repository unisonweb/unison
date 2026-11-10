``` unison
List.neighbors : [a] -> [(a, a)]
List.neighbors xs = List.slidingPairs xs

test> List.neighbors.tests.example = test.verify do
  ensureEqual (List.neighbors [1, 2, 3, 4]) [(1, 2), (2, 3), (3, 4)]
```
``` ucm
scratch/main> add
scratch/main> test
```

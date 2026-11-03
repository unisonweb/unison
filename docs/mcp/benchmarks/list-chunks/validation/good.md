``` unison
List.chunks : Nat -> [a] -> [[a]]
List.chunks = List.chunk

test> List.chunks.tests.examples = test.verify do
  ensureEqual (List.chunks 2 [1, 2, 3, 4, 5]) [[1, 2], [3, 4], [5]]
  ensureEqual (List.chunks 3 [1, 2, 3]) [[1, 2, 3]]
  ensureEqual (List.chunks 10 [1, 2]) [[1, 2]]
  ensureEqual (List.chunks 2 ([] : [Nat])) []
```
``` ucm
scratch/main> add
scratch/main> test
```

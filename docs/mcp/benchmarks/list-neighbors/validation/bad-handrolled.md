``` unison
List.neighbors : [a] -> [(a, a)]
List.neighbors xs =
  go acc = cases
    a +: (b +: rest) -> go (acc :+ (a, b)) (b +: rest)
    _ -> acc
  go [] xs

test> List.neighbors.tests.example = test.verify do
  ensureEqual (List.neighbors [1, 2, 3, 4]) [(1, 2), (2, 3), (3, 4)]
  ensureEqual (List.neighbors [1]) []
```
``` ucm
scratch/main> add
scratch/main> test
```

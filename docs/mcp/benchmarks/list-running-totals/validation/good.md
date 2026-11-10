``` unison
List.runningTotals : [Nat] -> [Nat]
List.runningTotals xs = List.drop 1 (List.Nonempty.toList (List.scanLeft (Nat.+) 0 xs))

test> List.runningTotals.tests.example = test.verify do
  ensureEqual (List.runningTotals [1, 2, 3, 4]) [1, 3, 6, 10]
  ensureEqual (List.runningTotals [5, 0, 2]) [5, 5, 7]
  ensureEqual (List.runningTotals []) []
  ensureEqual (List.runningTotals [7]) [7]
  ensureEqual (List.last (List.runningTotals (List.range 0 100))) (Some 4950)
```
``` ucm
scratch/main> add
scratch/main> test
```

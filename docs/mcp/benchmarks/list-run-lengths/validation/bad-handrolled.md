``` unison
List.runLengths : [a] -> [(a, Nat)]
List.runLengths xs =
  go acc = cases
    [] -> acc
    x +: rest -> match List.last acc with
      Some (y, n) | x === y -> go (List.take (List.size acc - 1) acc :+ (y, n + 1)) rest
      _ -> go (acc :+ (x, 1)) rest
  go [] xs

test> List.runLengths.tests.example = test.verify do
  ensureEqual (List.runLengths ["a", "a", "a", "b", "c", "c", "a", "a"]) [("a", 3), ("b", 1), ("c", 2), ("a", 2)]
  ensureEqual (List.runLengths ([] : [Nat])) []
```
``` ucm
scratch/main> add
scratch/main> test
```

``` unison
List.runLengths : [a] -> [(a, Nat)]
List.runLengths xs =
  step acc x = match List.last acc with
    Some (y, n) | y === x -> List.take (List.size acc - 1) acc :+ (y, n + 1)
    _ -> acc :+ (x, 1)
  List.foldLeft step [] xs

test> List.runLengths.tests.example = test.verify do
  ensureEqual (List.runLengths ["a", "a", "a", "b", "c", "c", "a", "a"]) [("a", 3), ("b", 1), ("c", 2), ("a", 2)]
  ensureEqual (List.runLengths ([] : [Nat])) []
```
``` ucm
scratch/main> add
scratch/main> test
```

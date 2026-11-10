``` unison
List.runLengths : [a] -> [(a, Nat)]
List.runLengths xs =
  List.map (g -> (List.Nonempty.head g, List.Nonempty.size g)) (List.groupConsecutive xs)

test> List.runLengths.tests.example = test.verify do
  ensureEqual (List.runLengths ["a", "a", "a", "b", "c", "c", "a", "a"]) [("a", 3), ("b", 1), ("c", 2), ("a", 2)]
  ensureEqual (List.runLengths [1, 1, 2, 3, 3, 3, 1]) [(1, 2), (2, 1), (3, 3), (1, 1)]
  ensureEqual (List.runLengths ["x"]) [("x", 1)]
  ensureEqual (List.runLengths ([] : [Nat])) []
  ensureEqual (List.size (List.runLengths (List.map (n -> n / 10) (List.range 0 100)))) 10
```
``` ucm
scratch/main> add
scratch/main> test
```

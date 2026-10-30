``` unison
List.runLengths : [a] -> [(a, Nat)]
List.runLengths xs = List.map (g -> (List.Nonempty.head g, List.Nonempty.size g)) (List.groupConsecutive xs)

test> List.runLengths.tests.example = test.verify do
  ensureEqual (List.runLengths ["a", "a", "a", "b", "c", "c", "a", "a"]) [("a", 3), ("b", 1), ("c", 2), ("a", 2)]
```
``` ucm
scratch/main> add
scratch/main> test
```

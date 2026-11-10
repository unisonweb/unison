``` unison
shuffleSeeded : Nat -> [a] -> [a]
shuffleSeeded seed xs =
  step s _ = Nat.mod (s Nat.* 1103515245 Nat.+ 12345) 2147483648
  keys = List.drop 1 (List.Nonempty.toList (List.scanLeft step seed xs))
  pairs = List.zip keys xs
  List.map at2 (List.sortBy at1 pairs)

test> shuffleSeeded.tests.example = test.verify do
  xs = List.range 0 20
  ensureEqual (shuffleSeeded 1 xs) (shuffleSeeded 1 xs)
  ensureEqual (List.sort (shuffleSeeded 7 xs)) xs
```
``` ucm
scratch/main> add
scratch/main> test
```

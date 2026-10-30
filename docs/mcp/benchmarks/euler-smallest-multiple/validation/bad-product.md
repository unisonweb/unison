``` unison
smallestMultiple : Nat -> Optional Nat
smallestMultiple n =
  if n === 0 then None else Some (List.foldLeft (Nat.*) 1 (List.range 1 (n + 1)))

test> smallestMultiple.tests.example = test.verify do
  ensureEqual (smallestMultiple 0) None
  ensureEqual (smallestMultiple 1) (Some 1)
  ensureEqual (smallestMultiple 3) (Some 6)
```
``` ucm
scratch/main> add
scratch/main> test
```

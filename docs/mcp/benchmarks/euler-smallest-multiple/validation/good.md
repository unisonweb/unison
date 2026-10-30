``` unison
smallestMultiple : Nat -> Optional Nat
smallestMultiple n =
  Abort.toOptional! do
    if n === 0 then Abort.abort else List.foldLeft Nat.lcm 1 (List.range 1 (n + 1))

test> smallestMultiple.tests.example = test.verify do
  ensureEqual (smallestMultiple 0) None
  ensureEqual (smallestMultiple 1) (Some 1)
  ensureEqual (smallestMultiple 10) (Some 2520)
  ensureEqual (smallestMultiple 20) (Some 232792560)
  ensureEqual (smallestMultiple 40) (Some 5342931457063200)
```
``` ucm
scratch/main> add
scratch/main> test
```

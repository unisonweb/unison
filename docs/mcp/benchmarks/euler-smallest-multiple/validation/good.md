``` unison
smallestMultiple : Nat -> Nat
smallestMultiple n =
  Optional.getOrBug "lcm" (Abort.toOptional! do List.foldLeft Nat.lcm 1 (List.range 1 (n + 1)))

test> smallestMultiple.tests.example = test.verify do
  ensureEqual (smallestMultiple 1) 1
  ensureEqual (smallestMultiple 10) 2520
  ensureEqual (smallestMultiple 20) 232792560
  ensureEqual (smallestMultiple 40) 5342931457063200
```
``` ucm
scratch/main> add
scratch/main> test
```

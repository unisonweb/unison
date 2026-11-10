``` unison
ability Counter where
  next : () -> Nat

Counter.doc = {{ A source of numbers: each {Counter.next} is one more than the last. Handle it with {Counter.from}. }}

Counter.from.doc = {{ {{ docExample 2 (do (start computation -> Counter.from start computation)) }} runs `computation`, answering each {Counter.next} with `start`, `start + 1`, and so on, and returns its result. Other abilities the computation uses pass through. }}
Counter.from : Nat -> '{g, Counter} r ->{g} r
Counter.from start computation =
  go : Nat -> Request {Counter} r ->{g} r
  go n = cases
    { pure } -> pure
    { Counter.next _ -> resume } -> handle resume n with go (n + 1)
  handle computation() with go start

test> Counter.tests.from = test.verify do
  ensureEqual (Counter.from 10 do (Counter.next(), Counter.next(), Counter.next())) (10, 11, 12)
  ensureEqual (Counter.from 0 do List.map (_ -> Counter.next()) [1, 2, 3, 4]) [0, 1, 2, 3]
  ensureEqual (Counter.from 5 do "no counting") "no counting"
```
``` ucm
scratch/main> add
scratch/main> test
scratch/main> docs Counter.from
```

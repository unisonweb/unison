``` unison
sampleSeeded : Nat -> Nat -> [a] -> [a]
sampleSeeded seed n xs = List.take n (Random.splitmix seed do Random.shuffle xs)

test> sampleSeeded.tests.examples = test.verify do
  ensureEqual (sampleSeeded 7 2 [10, 20, 30, 40]) (sampleSeeded 7 2 [10, 20, 30, 40])
  ensureEqual (List.size (List.distinct (sampleSeeded 7 2 [10, 20, 30, 40]))) 2
  ensureEqual (List.sort (sampleSeeded 3 10 [1, 2, 3])) [1, 2, 3]
  ensureEqual (sampleSeeded 1 0 [1, 2, 3]) []
  ensureEqual (sampleSeeded 1 2 ([] : [Nat])) []
```
``` ucm
scratch/main> add
scratch/main> test
```

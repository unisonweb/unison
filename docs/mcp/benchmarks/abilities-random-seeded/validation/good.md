``` unison
shuffleSeeded : Nat -> [a] -> [a]
shuffleSeeded seed xs = Random.splitmix seed do Random.shuffle xs

test> shuffleSeeded.tests.example = test.verify do
  xs = List.range 0 20
  ensureEqual (shuffleSeeded 1 xs) (shuffleSeeded 1 xs)
  ensureEqual (List.sort (shuffleSeeded 7 xs)) xs
  ensure (Set.size (Set.fromList (List.map (s -> shuffleSeeded s xs) (List.range 1 11))) >= 5)
  ensureEqual (shuffleSeeded 3 ([] : [Nat])) []
  ensureEqual (shuffleSeeded 3 ["x"]) ["x"]
```
``` ucm
scratch/main> add
scratch/main> test
```

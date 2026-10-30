```` unison
shuffleSeeded.doc = {{
{{ docExample 2 (do (seed xs -> shuffleSeeded seed xs)) }} returns a random permutation of `xs`, chosen by `seed`: the same seed and list always give the same result, and different seeds usually give different results. It uses no IO. For example:

```
shuffleSeeded 1 [1, 2, 3, 4, 5]
```

The result has the same elements as the input, whatever the seed:

```
List.sort (shuffleSeeded 42 [5, 3, 1, 4, 2])
```

*Also see:* {Random.shuffle}, {Random.splitmix}
}}

shuffleSeeded : Nat -> [a] -> [a]
shuffleSeeded seed xs =
  go = do
    arr = mutable.Array.fromList xs
    n = mutable.Array.size arr
    swap i j =
      tmp = mutable.Array.read arr i
      mutable.Array.write arr i (mutable.Array.read arr j)
      mutable.Array.write arr j tmp
    List.foreach (i -> swap i (Random.natIn 0 (i + 1))) (List.reverse (List.range 1 n))
    List.map (i -> mutable.Array.read arr i) (List.range 0 n)
  Random.splitmix seed do Exception.unsafeRun! do Scope.run go

test> shuffleSeeded.tests.example = test.verify do
  xs = List.range 0 20
  ensureEqual (shuffleSeeded 1 xs) (shuffleSeeded 1 xs)
  ensureEqual (List.sort (shuffleSeeded 7 xs)) xs
  ensure (Set.size (Set.fromList (List.map (s -> shuffleSeeded s xs) (List.range 1 11))) >= 5)
  ensureEqual (shuffleSeeded 3 ([] : [Nat])) []
  ensureEqual (shuffleSeeded 3 ["x"]) ["x"]
````
``` ucm
scratch/main> add
scratch/main> test
scratch/main> docs shuffleSeeded
```

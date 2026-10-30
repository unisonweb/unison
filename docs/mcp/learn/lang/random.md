Random numbers, shuffles and random choices, with the `Random` ability. Run it from a seed to get the same answers every time, or from IO to get different ones.

- `Random.natIn lo hi` is a random `Nat` from `lo` up to but not including `hi`, `Random.nat()` any `Nat`, `Random.boolean()` a `Boolean`, and `Random.shuffle list` is a random reordering of a list. `Random.listOf (do <length>) (do <element>)` makes a list of random length with random elements. The operations are [abilities](learn:lang/abilities) used inside a handler.
- **From a seed, the answers are always the same:** `Random.splitmix seed do <computation>` runs the computation with a generator started from a `Nat` seed and returns its result as a plain value, so a function that uses it has a pure type. `Random.splitmix 7 do Random.shuffle [1, 2, 3, 4, 5]` is always the same shuffle.
- For a different answer each time, use `Random.run` inside `IO` (see [running code](learn:ucm/run)).
- Find what the ability offers with `find.all Random.` or `docs Random`, rather than writing your own generator or shuffle.
- To use random numbers in tests (many random inputs), see [property-based tests](learn:testing/props).

## Detail

**A seeded function.**

````
``` unison
pickTwo : Nat -> [a] -> [a]
pickTwo seed xs = List.take 2 (Random.splitmix seed do Random.shuffle xs)

> pickTwo 7 [1, 2, 3, 4, 5]
> pickTwo 7 [1, 2, 3, 4, 5]
```
````

Both lines give the same two elements.

**Nesting handlers.** `Random.splitmix seed do Exception.unsafeRun! do Scope.run do ...` runs code that uses randomness, can fail, and uses mutable state (see [mutable state](learn:lang/mutable)).

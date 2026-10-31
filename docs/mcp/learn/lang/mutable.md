Mutable state, for when a function needs a variable that several steps update, such as a running total, a counter or a buffer, without threading it through every call.

- Use `Scope.run do ...` to make a place where mutable values live. Inside it, `Scope.ref initial` creates a mutable `Ref`, `Ref.read ref` reads it, `Ref.write ref value` writes it, and `Scope.ref.modify ref f` changes it with `f`.
- **`Scope.run` is a handler.** Its result is an ordinary value, and a function that uses it has a pure type: `count : [a] -> Nat`, not `count : [a] ->{Scope s} Nat`. A `Ref` can't escape from the scope it was made in.
- Create the `Ref` once, outside a recursive helper, and let the helper read and write it. Its signature is `go : Ref {Scope s} Nat -> Tree a ->{Scope s} Tree (Nat, a)`: the `Ref` takes the ability in braces, with the same `s`. Don't pass a counter in and out of every recursive call (`go : Nat -> Tree a -> (Nat, Tree b)`) when a `Ref` does it: that is the whole point.
- Effects happen in the order the code runs, top to bottom, and left to right within a call. A `List.map` or `List.foreach` over a function that writes a `Ref` runs it for the elements in list order.
- For a mutable array, `Scope.Raw.array n` makes one of size `n`, and `mutable.Array.Raw.read array i` and `mutable.Array.Raw.write array i value` use it. They fail with an `Exception` if the index is out of range, so wrap the scope in `Exception.unsafeRun!` when you have checked the indexes yourself.
- `Store` is a lighter ability for a single piece of state: `Store.get`, `Store.put`, `Store.modify f`, handled by `Store.withInitialValue initial do ...`. It works, but a `Scope` with a `Ref` is more direct and what to reach for in a pure function.

## Detail

**A running total in a pure function.**

````
``` unison
sumOdd : [Nat] -> Nat
sumOdd xs = Scope.run do
  total = Scope.ref 0
  List.foreach (x -> if Nat.isOdd x then Scope.ref.modify total (t -> t + x) else ()) xs
  Ref.read total

> sumOdd [1, 2, 3, 4, 5]
```
````

This gives `9`.

**An array.** Swapping the ends of a list through an array:

````
``` unison
swapEnds : [a] -> [a]
swapEnds xs = Exception.unsafeRun! do Scope.run do
  n = List.size xs
  arr = Scope.Raw.array n
  List.foreach (cases (x, i) -> mutable.Array.Raw.write arr i x) (List.indexed xs)
  first = mutable.Array.Raw.read arr 0
  last = mutable.Array.Raw.read arr (n - 1)
  mutable.Array.Raw.write arr 0 last
  mutable.Array.Raw.write arr (n - 1) first
  List.map (i -> mutable.Array.Raw.read arr i) (List.range 0 n)

> swapEnds [1, 2, 3, 4]
```
````

This gives `[4, 2, 3, 1]`. (For an empty list `n - 1` is `0` for a `Nat`, and the read fails: check the size first if that can happen.)

**Mutable values and abilities.** Combine `Scope.run` with the other handlers by nesting: `Random.splitmix seed do Exception.unsafeRun! do Scope.run do ...` runs code that uses randomness and an array (see [abilities](learn:lang/abilities)).

**Docs.** `docs Scope.run`, `docs Ref` and `docs mutable.Array.Raw` have more (see [find](learn:ucm/find)).

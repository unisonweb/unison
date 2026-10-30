A list is an immutable sequence of elements of one type, written `[a, b, c]` with type `[t]`. Adding an element to either end is fast, so build lists in whatever order is natural.

- `[]` is the empty list. `x +: list` adds `x` at the front and `list :+ x` adds it at the end. Both take constant time, because a list is a finger tree. `++` joins two lists, in logarithmic time.
- **Build a list in the order you want it.** To accumulate results in a recursive function or fold, add to the end of the accumulator with `:+`. Don't build it backwards with `+:` and `List.reverse` it at the end, which is a habit from other languages that is unnecessary here.
- `List.size list` is its length. `List.head`, `List.last`, `List.tail` and `List.init` give an `Optional` (`None` for an empty list), and so does `List.at index list`. `List.take n list` and `List.drop n list` split off a prefix.
- To transform or search: `List.map f list`, `List.filter pred list`, `List.foldLeft (acc a -> ...) initial list`, `List.flatMap f list`, `List.zip as bs`, `List.any pred list`, `List.all pred list`, `List.contains x list`.
- **Base has more than you'd guess, so search before you write a loop** (`find.all <word>`, see [reuse](learn:skills/reuse)). Common shapes that already exist: `List.filterMap f list` (map and keep the `Some`s), `List.partition`, `List.slidingPairs list` (each element with the next) and `List.slidingWindow`, `List.groupConsecutive list` (runs of equal neighbors) and `List.groupBy`, `List.scanLeft` (every intermediate result of a fold, running totals), `List.zip`, `List.tails`, `List.distinct`, `List.count`. When the task says "each element and the next", "runs of equal elements" or "running ...", one of these is probably the whole solution. If a function already does exactly what you were asked for (check its `docs`), define yours as that function (`List.xf = List.map`, or `xf f xs = List.map f xs`) and test it, instead of writing the loop again.
- Take a list apart with [patterns](learn:lang/cases): `[]` (empty), `[a]` and `[a, b]` (exactly that many), `head +: tail` (first element and the rest), `init :+ last` (all but the last, and the last), `first +: middle :+ last`, and `[a, b] ++ rest` (the first two, then the rest). `a +: b +: rest` doesn't work, because `b` has to be a list there. Use `[a, b] ++ rest`.

## Detail

**A recursive function over a list,** building its result as it goes:

````
``` unison
doubleAll : [Nat] -> [Nat] -> [Nat]
doubleAll acc = cases
  [] -> acc
  h +: t -> doubleAll (acc :+ (h * 2)) t

> doubleAll [] [1, 2, 3]
```
````

This gives `[2, 4, 6]`. If `:+` is reported as ambiguous, write `use List :+` first. `List.map (x -> x * 2) list` does the same without recursion.

**Making lists.** `List.fill n x` is `n` copies of `x`, `List.tabulate n f` is `f 0`, `f 1`, up to `f (n - 1)`, `Nat.range lo hi` is the numbers from `lo` up to but not including `hi` (`Nat.rangeClosed` includes `hi`, and `Int.range` is the same for `Int`s), `List.singleton x` is `[x]`, and `List.replicate n do <computation>` runs a computation `n` times and collects the results (see [delayed computations](learn:lang/delayed)).

**Changing a list.** `insertAt index x list`, `deleteAt index list`, `replace index x list` and `List.intersperse x list`. Indexes start at `0`. Insertion and deletion in the middle take logarithmic time.

**Looking at a list.** `List.takeWhile pred list`, `List.dropWhile pred list`, `List.maximum list` and `List.minimum list` (both `Optional`), `List.slice from to list`, `List.splitAt index list`, `List.span pred list`, `List.halve list`.

**Combining.** `List.join lists` concatenates a list of lists, `intercalate separator lists` puts a list between them, and `List.zip` pairs up two lists.

**Sorting and rearranging.** `Heap.sort list` sorts ascending, `sortDescending list` descending, and `sortBy f list` by a property. `distinct list` removes duplicates, `List.reverse list` flips the order, `List.tails list` and `inits list` give all the suffixes and prefixes.

**Traversing.** `mapIndexed (n x -> ...) list` passes the index too, `List.foreach f list` runs a function for its [abilities](learn:lang/abilities) only, `List.foldRight` goes from the right, and `List.foldMap f combine initial list` maps and combines. `List.scanLeft` is like `foldLeft` but returns every intermediate result, as a `List.Nonempty` (a list that can't be empty, from `toNonempty list`).

**Converting.** `Set.fromList` and `Set.toList`, `Map.fromList` and `Map.toList` (see [maps](learn:lang/collections/map)), `fromCharList` and `toCharList` for `Text`, and `Stream.toList do ...` for a `Stream`.

**More.** `docs data.List` has all of the above with examples, and the docs of each function have more (see [find](learn:ucm/find)).

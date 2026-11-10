The common collections and the functions you reach for first.

- A [list](learn:lang/collections/list) is `[1, 2, 3]`, with type `[Nat]`. It holds one type of element. Adding to either end is fast.
- A [map](learn:lang/collections/map) from keys to values has no literal: `Map.fromList [(1, "a"), (2, "b")]`, then `Map.get key map`.
- `Optional a` is `Some value` or `None`, for a value that may be missing.
- `Set a` is `Set.fromList [1, 2, 3]`, with `Set.contains x set` and `Set.union`.
- `Text` is joined with `++` and measured with `Text.size`.
- Comparing elements of a type variable: `<` and `==` belong to a type (`Nat.<`), so a generic function uses `Universal.lt`, `===`, or `Universal.ordering` (`a -> a -> Ordering`, what `List.maximumBy` and the sorting functions take); see [operators](learn:lang/operators).
- `docs List` (or `docs Map`, `docs Set`) shows what is available (see [find](learn:ucm/find)). The full API is in the base library (see [libraries](learn:ucm/libraries)).

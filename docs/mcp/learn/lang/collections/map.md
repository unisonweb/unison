A `Map k v` is a finite map from keys to values, sorted by key. Looking up, inserting and deleting a key take logarithmic time.

- It has no literal. `Map.fromList [("apple", 1), ("pear", 2)]` builds one from a list of key-value pairs, and `Map.empty` is the empty map. Each key appears once: when a key is repeated, the later value wins.
- `Map.get key map` is `Some value` or `None`, and `Map.getOrElse default key map` gives `default` for a missing key. The key comes before the map in every function, so these also work as `map |> Map.get key`.
- Maps are immutable. `Map.insert key value map` and `Map.delete key map` return a new map. `Map.contains key map` and `Map.size map` ask questions.
- `Map.toList map` is the pairs, in key order. `Map.keys map` and `Map.values map` are lists.
- `Map.map f map` changes the values. `Map.union a b` combines two maps, and `Map.unionWith f a b` uses `f` to combine the values of keys in both. `Map.alter f key map` changes one entry: `f` is given the `Optional` current value and returns the `Optional` new one.
- **To build or walk a map, use these, not a recursive helper:** `Map.fromListWith f pairs` is `Map.fromList` with `f` to combine the values of a repeated key, and `Map.foldLeftWithKey (acc k v -> ...) initial map` goes through the entries in key order.
- Keys are compared with `Universal.ordering`, so any type can be a key.
- **Equal maps can differ in structure,** so `===` and `ensureEqual` can say that two equal maps aren't. Compare with `a Map.== b`, or compare `Map.toList` of each.
- To take a map apart, use `Map.get` with [`match`](learn:lang/cases), or `Map.toList` and match on the list. There are no map patterns.

## Detail

**An example.**

````
``` unison
fruit : Map Text Nat
fruit = Map.fromList [("apple", 1), ("pear", 2)]

> Map.get "pear" fruit
> Map.toList (Map.insert "fig" 3 fruit)
> Map.getOrElse 0 "kiwi" fruit
> Map.alter (o -> Some (Optional.getOrElse 0 o Nat.+ 1)) "apple" fruit |> Map.toList
```
````

These give `Some 2`, the three pairs, `0`, and `apple` with `2`.

**Counting.** `Map.alter (o -> Some (Optional.getOrElse 0 o Nat.+ 1)) key counts` increments a count, starting from `0`.

**How it works.** A map is a balanced binary tree, ordered by key, with the size of each subtree stored, so most operations on one key are logarithmic in the size.

**More.** `docs data.Map` has examples, and the namespace `Map` has many more functions (see [find](learn:ucm/find)).


# Testing functions that use the base library

``` ucm
scratch/main> lib.install @unison/base/releases/3.35.0
```

This just verifies that a `Map` prints out nicely, as a call to `Map.fromList`:

```unison
> Map.fromList [("Alice", 1), ("Bob", 2), ("Carol", 3)]

> Map.fromList (List.range 0 25 |> List.map (i -> (i,i)))
```
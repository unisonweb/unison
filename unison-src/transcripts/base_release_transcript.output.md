# Testing functions that use the base library

``` ucm
scratch/main> lib.install @unison/base/releases/3.35.0

  I installed @unison/base/releases/3.35.0 into
  lib.unison_base_3_35_0
```

This just verifies that a `Map` prints out nicely, as a call to `Map.fromList`:

``` unison
> Map.fromList [("Alice", 1), ("Bob", 2), ("Carol", 3)]

> Map.fromList (List.range 0 25 |> List.map (i -> (i,i)))
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.

    1 | > Map.fromList [("Alice", 1), ("Bob", 2), ("Carol", 3)]
          ⧩
          Map.fromList [("Alice", 1), ("Bob", 2), ("Carol", 3)]

    3 | > Map.fromList (List.range 0 25 |> List.map (i -> (i,i)))
          ⧩
          Map.fromList
            [ (0, 0)
            , (1, 1)
            , (2, 2)
            , (3, 3)
            , (4, 4)
            , (5, 5)
            , (6, 6)
            , (7, 7)
            , (8, 8)
            , (9, 9)
            , (10, 10)
            , (11, 11)
            , (12, 12)
            , (13, 13)
            , (14, 14)
            , (15, 15)
            , (16, 16)
            , (17, 17)
            , (18, 18)
            , (19, 19)
            , (20, 20)
            , (21, 21)
            , (22, 22)
            , (23, 23)
            , (24, 24)
            ]
```

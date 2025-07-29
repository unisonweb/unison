``` ucm :hide
scratch/main> builtins.merge
```

This should render as `Bytes.fromList [1,2,3,4]`, not `##Bytes.fromSequence [1,2,3,4]`:

``` unison
> Bytes.fromList [1,2,3,4]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.

    1 | > Bytes.fromList [1,2,3,4]
          ⧩
          0xs01020304
```

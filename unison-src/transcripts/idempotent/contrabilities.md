``` ucm :hide
scratch/main> builtins.merge
```

``` unison
f : (() -> a) -> Nat
f x = 42
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `update`, here's how your codebase would change:

    ⍟ These new definitions are ok to `update`:
    
      f : '{g} a -> Nat
```

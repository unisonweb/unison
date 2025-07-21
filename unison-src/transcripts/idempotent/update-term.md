``` ucm
scratch/main> builtins.merge

  Done.
```

``` unison
foo : Nat
foo = 5
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + foo : Nat

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
foo : Nat
foo = 6
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ foo : Nat

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> view foo

  foo : Nat
  foo = 6
```

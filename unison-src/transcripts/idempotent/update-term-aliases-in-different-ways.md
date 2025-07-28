``` ucm
scratch/main> builtins.merge

  Done.
```

``` unison
foo : Nat
foo = 5

bar : Nat
bar = 5
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + bar : Nat
  + foo : Nat

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

bar : Nat
bar = 7
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ bar : Nat
  ~ foo : Nat

  ~ (modified)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> view foo bar

  bar : Nat
  bar = 7

  foo : Nat
  foo = 6
```

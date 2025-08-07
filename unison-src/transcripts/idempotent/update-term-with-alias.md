``` ucm
> builtins.merge

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
> add

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
      (was also named bar)

  ~ (modified)

  Run `update` to apply these changes to your codebase.
```

``` ucm
> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

> view foo bar

  bar : Nat
  bar = 5

  foo : Nat
  foo = 6
```

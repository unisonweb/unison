``` ucm
scratch/main> builtins.merge

  Done.
```

``` unison
test> foo = []
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + foo : [Result]

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.

    1 | test> foo = []
    
```

After adding the test `foo`, we expect `view` to render it like a test. (Bug: It doesn't.)

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> view foo

  foo : [Result]
  foo = []
```

``` unison
foo = 1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ foo : Nat

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

After updating `foo` to not be a test, we expect `view` to not render it like a test.

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> view foo

  foo : Nat
  foo = 1
```

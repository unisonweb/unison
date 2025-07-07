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

  I found and typechecked these definitions in scratch.u. If you
  do an `update`, here's how your codebase would change:

    ⍟ New definitions:
    
      bar : Nat
      foo : Nat
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

  I found and typechecked these definitions in scratch.u. If you
  do an `update`, here's how your codebase would change:

    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      foo : Nat
        (The old definition is also named bar.)
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> view foo bar

  bar : Nat
  bar = 5

  foo : Nat
  foo = 6
```

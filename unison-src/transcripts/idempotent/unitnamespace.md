``` unison
`()`.foo = "bar"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + `()`.foo : ##Text

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> find

  1. `()`.foo : ##Text

scratch/main> find-in `()`

  1. foo : ##Text

scratch/main> delete.namespace `()`

  Done.
```

``` unison
`()`.foo = "bar"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `update`, here's how your codebase would change:

    ⍟ These new definitions are ok to `update`:
    
      `()`.foo : ##Text
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

``` unison
`()`.foo = "bar"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + `()`.foo : ##Text

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

> find

  1. `()`.foo : ##Text

> find-in `()`

  1. foo : ##Text

> delete.namespace `()`

  Done.
```

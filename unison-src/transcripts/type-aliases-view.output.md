# view of a type alias

Confirms that after adding a `type alias`, `view <alias-name>` round-trips it
back to its source form.

``` ucm :hide
> builtins.mergeio
```

``` unison
type alias Endo a = a -> a
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type alias Endo a = a -> a

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Done.

> view Endo

  type alias Endo a = a -> a
```

The expected output: `view Endo` renders the alias as
`type alias Endo a = a -> a`. Until the alias-aware show/load path lands,
this stanza errors out because the lookup treats every type-position ref as
a decl.

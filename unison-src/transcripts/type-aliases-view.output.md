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

  No changes found.
```

``` ucm
> add
> view Endo
```

🛑

The transcript failed due to an error in the stanza above. The error is:

``` 
⚠️

The following names were not found in the codebase. Check your spelling.
  Endo
```

# Cross-file type alias resolution

Confirms that a `type alias` declared in one file can be referenced from a
later, separate scratch file.

``` ucm :hide
> builtins.mergeio
```

First file: declare the alias and add it to the codebase.

``` unison
type alias Endo a = a -> a
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.
```

``` ucm
> add

  Done.
```

Second file: reference the alias from a fresh scratch file. The parser
should look up `Endo` in the namespace, fetch its body from the codebase,
and expand inline before hashing.

``` unison
g : Endo Nat
g x = x + 2
```

🛑

The transcript failed due to an error in the stanza above. The error is:

``` 

  ❓
  
  I couldn't resolve any of these names:
  
      1 | g : Endo Nat
  
  
  Name   Type   Suggestions
                
  Endo   type   No matches
```

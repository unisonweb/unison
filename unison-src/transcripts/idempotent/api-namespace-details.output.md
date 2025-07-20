# Namespace Details Test

``` ucm :hide
scratch/main> builtins.mergeio
```

``` unison
{{ Documentation }}
nested.names.x = 42

nested.names.readme = {{
Here's a *README*!
}}
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `update`, here's how your codebase would change:

    ⍟ New definitions:
    
      nested.names.readme : Doc2
      nested.names.x      : Nat
      nested.names.x.doc  : Doc2
```

``` ucm
scratch/main> add
  Okay, I'm searching the branch for code that needs to be
  updated...
  Done.
```

🛑

The transcript failed due to an error in the stanza above. The error is:

``` 
This branch has more than one term with the name
`builtin.ImmutableByteArray.fromBytes`. Please delete or rename
all but one of them, then try the update again.
```

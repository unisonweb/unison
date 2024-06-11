```unison
foo = "bar"
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo : ##Text

```
```ucm
  ☝️  The namespace .`()` is empty.

.`()`> add

  ⍟ I've added these definitions:
  
    foo : ##Text

scratch/main> find

  ☝️
  
  I couldn't find matches in this namespace, searching in
  'lib'...

  😶
  
  No results. Check your spelling, or try using tab completion
  to supply command arguments.
  
  `find.global` can be used to search outside the current
  namespace.

```

```ucm
.`()`> addscratch/main> findscratch/main> find-in `()`scratch/main> delete.namespace `()`
```


🛑

The transcript failed due to an error in the stanza above. The error is:


  😶
  
  No results. Check your spelling, or try using tab completion
  to supply command arguments.
  
  `find.global` can be used to search outside the current
  namespace.


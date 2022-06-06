```unison
foo = "bar"
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo : ##Text

```
```ucm
.> cd ()

  ☝️  The namespace .() is empty.

.()> add

  ⍟ I've added these definitions:
  
    foo : ##Text

.> delete.namespace ()

  Done.

```

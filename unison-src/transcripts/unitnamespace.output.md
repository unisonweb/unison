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

.> find

  1. `()`.foo : ##Text
  

.> find-in `()`

  1. foo : ##Text
  

.> delete.namespace `()`

  Done.

```

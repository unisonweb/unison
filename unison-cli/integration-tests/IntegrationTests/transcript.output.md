# Integration test: transcript

Test

```unison
use .builtin

coolFunction x = x * 2

coolFunction.doc = [: This is a cool function. :]
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      coolFunction     : Nat -> Nat
      coolFunction.doc : Doc.Deprecated

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    coolFunction     : Nat -> Nat
    coolFunction.doc : Doc.Deprecated

.> link coolFunction.doc coolFunction

  Updates:
  
    1. coolFunction : Nat -> Nat
       + 2. coolFunction.doc : builtin.Doc

```

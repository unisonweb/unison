#### Big list crash

Big lists have been observed to crash, while in the garbage collection step.

```unison
a = 0
x = 1 + a
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      a : Nat
      x : Nat
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```

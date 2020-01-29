#### Big list crash

Big lists have been observed to crash, while in the garbage collection step.

```unison
unique type Direction = U | D | L | R

a = 0
x = [a+1]
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      unique type Direction
      a : Nat
      x : [Nat]
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```

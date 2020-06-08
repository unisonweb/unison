# Tracing Unison programs

```unison
fac n = 
  if n < 2 then
    trace "done" 1
  else 
    trace "next" ()
    n * fac (drop 1 n)

> fac 4
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      fac : Nat -> Nat
  
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

"next"
"next"
"next"
"done"

    8 | > fac 10
          ⧩
          10

```

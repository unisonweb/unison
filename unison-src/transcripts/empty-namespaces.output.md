# Empty namespace behaviours

```unison
mynamespace.x = 1
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      mynamespace.x : Nat

```
The deleted namespace shouldn't appear in `ls` output.
```ucm
.> ls

  1. builtin/     (381 definitions)
  2. mynamespace/ (0 definitions)

```

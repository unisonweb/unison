# Update on conflict

```unison
a.x = 1
b.x = 2
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      a.x : Nat
      b.x : Nat

```
Cause a conflict:
```ucm
scratch/main> add

  ⍟ I've added these definitions:
  
    a.x : Nat
    b.x : Nat

.merged> merge.old .a

  ⚠️
  
  The namespace .a doesn't exist.

```

```ucm
scratch/main> add.merged> merge.old .a.merged> merge.old .b
```


🛑

The transcript failed due to an error in the stanza above. The error is:


  ⚠️
  
  The namespace .a doesn't exist.


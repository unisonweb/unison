# Merge loop test

This tests for regressions of https://github.com/unisonweb/unison/issues/1276 where trivial merges cause loops in the history.

Let's make three identical namespaces with different histories:

```unison
a = 1
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      a : ##Nat

```
```ucm
  ☝️  The namespace .x is empty.

.x> add

  ⍟ I've added these definitions:
  
    a : ##Nat

```
```unison
b = 2
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      b : ##Nat

```
```ucm
.x> add

  ⍟ I've added these definitions:
  
    b : ##Nat

```
```unison
b = 2
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked the definitions in scratch.u. This
  file has been previously added to the codebase.

```
```ucm
  ☝️  The namespace .y is empty.

.y> add

  ⍟ I've added these definitions:
  
    b : ##Nat

```
```unison
a = 1
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      a : ##Nat

```
```ucm
.y> add

  ⍟ I've added these definitions:
  
    a : ##Nat

```
```unison
a = 1
b = 2
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked the definitions in scratch.u. This
  file has been previously added to the codebase.

```
```ucm
  ☝️  The namespace .z is empty.

.z> add

  ⍟ I've added these definitions:
  
    a : ##Nat
    b : ##Nat

scratch/main> merge.old x y

  ⚠️
  
  The namespace x doesn't exist.

```

```ucm
.z> addscratch/main> merge.old x yscratch/main> merge.old y zscratch/main> history z
```


🛑

The transcript failed due to an error in the stanza above. The error is:


  ⚠️
  
  The namespace x doesn't exist.


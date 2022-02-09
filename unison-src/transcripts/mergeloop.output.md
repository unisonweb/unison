# Merge loop test

This tests for regressions of https://github.com/unisonweb/unison/issues/1276 where trivial merges cause loops in the history.

Let's make three identical namespaces with different histories:

```unison
a = 1
```

```ucm

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

  I found and typechecked the definitions in scratch.u. This
  file has been previously added to the codebase.

```
```ucm
  ☝️  The namespace .z is empty.

.z> add

  ⍟ I've added these definitions:
  
    a : ##Nat
    b : ##Nat

.> merge x y

  Nothing changed as a result of the merge.

.> merge y z

  Nothing changed as a result of the merge.

.> history z

  Note: The most recent namespace hash is immediately below this
        message.
  
  
  
  This segment of history starts with a merge. Use
  `history #som3n4m3space` to view history starting from a given
  namespace hash.
  
  ⊙ #b7fr6ifj87
  ⑃
  #9npggauqo9
  #dm4u1eokg1

```

First we make two changes to the codebase, so that there's more than one line
for the `reflog` command to display:

```unison
x = 1
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      x : Nat

```
```ucm
scratch/main> add

  ⍟ I've added these definitions:
  
    x : Nat

```
```unison
y = 2
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      y : Nat

```
```ucm
scratch/main> add

  ⍟ I've added these definitions:
  
    y : Nat

scratch/main> view y

  y : Nat
  y = 2

```
```ucm
scratch/main> reflog

  ⚠️
  
  The reflog is empty

```
If we `reset-root` to its previous value, `y` disappears.
```ucm
scratch/main> reset-root 2

```
```ucm
scratch/main> view y

  y : Nat
  y = 2

```

```



🛑

The transcript was expecting an error in the stanza above, but did not encounter one.

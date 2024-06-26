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
    
      x : ##Nat

```
```ucm
scratch/main> add

  ⍟ I've added these definitions:
  
    x : ##Nat

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
    
      y : ##Nat

```
```ucm
scratch/main> add

  ⍟ I've added these definitions:
  
    y : ##Nat

scratch/main> view y

  y : ##Nat
  y = 2

```
```ucm
scratch/main> reflog

  Here is a log of the root namespace hashes, starting with the
  most recent, along with the command that got us there. Try:
  
    `fork 2 .old`             
    `fork #7fp7j6976q .old`   to make an old namespace
                              accessible again,
                              
    `reset-root #7fp7j6976q`  to reset the root namespace and
                              its history to that of the
                              specified namespace.
  
       When   Root Hash     Action
  1.   now    #8ur19pdmaa   add
  2.   now    #7fp7j6976q   add
  3.          #sg60bvjo91   history starts here
  
  Tip: Use `diff.namespace 1 7` to compare namespaces between
       two points in history.

```
If we `reset-root` to its previous value, `y` disappears.
```ucm
scratch/main> reset-root 2

  Done.

```
```ucm
scratch/main> view y

  ⚠️
  
  The following names were not found in the codebase. Check your spelling.
    y

```

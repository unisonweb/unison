First we make two changes to the codebase, so that there's more than one line
for the `reflog` command to display:

```unison
x = 1
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      x : builtin.Nat
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    x : builtin.Nat

```
```unison
y = 2
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      y : builtin.Nat
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    y : builtin.Nat

.> view y

  y : builtin.Nat
  y = 2

```
```ucm
.> reflog

  Here is a log of the root namespace hashes, starting with the
  most recent, along with the command that got us there. Try:
  
    `fork 2 .old`             
    `fork #v4q504aibl .old`   to make an old namespace
                              accessible again,
                              
    `reset-root #v4q504aibl`  to reset the root namespace and
                              its history to that of the
                              specified namespace.
  
  1. #4ic0tv5m4s : add
  2. #v4q504aibl : add
  3. #ruf07f2h54 : (initial reflogged namespace)

```
If we `reset-root` to its previous value, `y` disappears.
```ucm
.> reset-root 2

  Done.

```
```ucm
.> view y

  ⚠️
  
  The following names were not found in the codebase. Check your spelling.
    y

```

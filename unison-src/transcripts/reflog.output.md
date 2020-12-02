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
    
      x : Nat

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    x : Nat

```
```unison
y = 2
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      y : Nat

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    y : Nat

.> view y

  y : Nat
  y = 2

```
```ucm
.> reflog

  Here is a log of the root namespace hashes, starting with the
  most recent, along with the command that got us there. Try:
  
    `fork 2 .old`             
<<<<<<< HEAD
    `fork #v05jsnqbib .old`   to make an old namespace
                              accessible again,
                              
    `reset-root #v05jsnqbib`  to reset the root namespace and
                              its history to that of the
                              specified namespace.
  
  1. #11nprfpqgo : add
  2. #v05jsnqbib : add
  3. #npsgjle324 : builtins.merge
=======
    `fork #5vgpg52rfo .old`   to make an old namespace
                              accessible again,
                              
    `reset-root #5vgpg52rfo`  to reset the root namespace and
                              its history to that of the
                              specified namespace.
  
  1. #83c6vvg501 : add
  2. #5vgpg52rfo : add
  3. #3662ae22bk : builtins.merge
>>>>>>> trunk
  4. #7asfbtqmoj : (initial reflogged namespace)

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

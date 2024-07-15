# Update on conflict

Updating conflicted definitions works fine.

``` unison
x = 1
temp = 2
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      temp : Nat
      x    : Nat

```
``` ucm
scratch/main> add

  ⍟ I've added these definitions:
  
    temp : Nat
    x    : Nat

scratch/main> debug.alias.term.force temp x

  Done.

scratch/main> delete.term temp

  Done.

```
``` unison
x = 3
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      x : Nat

```
``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> view x

  x : Nat
  x = 3

```

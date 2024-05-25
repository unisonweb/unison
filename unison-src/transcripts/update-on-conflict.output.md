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
.> add

  ⍟ I've added these definitions:
  
    a.x : Nat
    b.x : Nat

.merged> merge.old .a

  Here's what's changed in the current namespace after the
  merge:
  
  Added definitions:
  
    1. x : Nat
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

  Applying changes from patch...

.merged> merge.old .b

  Here's what's changed in the current namespace after the
  merge:
  
  New name conflicts:
  
    1. x#gjmq673r1v : Nat
       ↓
    2. ┌ x#dcgdua2lj6 : Nat
    3. └ x#gjmq673r1v : Nat
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

  Applying changes from patch...

```
Updating conflicted definitions works fine.

```unison
x = 3
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      x : Nat

```
```ucm
.merged> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

```

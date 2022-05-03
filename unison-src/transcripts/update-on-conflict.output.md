# Update on conflict

```unison
a.x = 1
b.x = 2
```

```ucm

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

  ☝️  The namespace .merged is empty.

.merged> merge .a

  Here's what's changed in the current namespace after the
  merge:
  
  Added definitions:
  
    1. x : Nat
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

.merged> merge .b

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

.> cd .

```
Ideally we could just define the canonical `x` that we want, and update
to accept it, but we can't:

```unison
x = 1 + 2
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      x : Nat

```
Update fails on conflicted `x`:

```ucm
.merged> update

  x These definitions failed:
  
    Reason
    conflicted   x   : Nat
  
    Tip: Use `help filestatus` to learn more.

```

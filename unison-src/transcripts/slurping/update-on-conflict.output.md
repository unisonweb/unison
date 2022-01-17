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
  
    1. x#jk19sm5bf8 : Nat
       ↓
    2. ┌ x#0ja1qfpej6 : Nat
    3. └ x#jk19sm5bf8 : Nat
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

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
  
    x These definitions would fail on `add` or `update`:
    
      Reason
      conflicted   x   : Nat
    
      Tip: Use `help filestatus` to learn more.

```
Update fails on conflicted `x`:

```ucm
.merged> update

  x These definitions failed:
  
    Reason
    conflicted   x   : Nat
  
    Tip: Use `help filestatus` to learn more.

```

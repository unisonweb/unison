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

```
Updating conflicted definitions works fine, and the associated patch contains two entries.

```unison
x = 3
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      x : Nat

```
```ucm
.merged> update

  ⍟ I've updated these names to your new definition:
  
    x : Nat

.merged> view.patch

  Edited Terms:
    1. b.x -> 3. x
    2. a.x -> 4. x
  
  Tip: To remove entries from a patch, use
       delete.term-replacement or delete.type-replacement, as
       appropriate.

```

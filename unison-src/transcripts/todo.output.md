# Test the `todo` command

## Simple type-changing update.

```unison
x = 1
useX = x + 10

type MyType = MyType Nat
useMyType = match MyType 1 with
  MyType a -> a + 10
```

Perform a type-changing update so dependents are added to our update frontier.

```unison
x = -1

type MyType = MyType Text
```

```ucm
.simple> update.old

  ⍟ I've updated these names to your new definition:
  
    type MyType
    x : Int

.simple> todo

  🚧
  
  The namespace has 2 transitive dependent(s) left to upgrade.
  Your edit frontier is the dependents of these definitions:
  
    type #vijug0om28
    #gjmq673r1v : Nat
  
  I recommend working on them in the following order:
  
  1. useMyType : Nat
  2. useX      : Nat
  
  

```
## A merge with conflicting updates.

```unison
x = 1
type MyType = MyType
```

Set up two branches with the same starting point.

Update `x` to a different term in each branch.

```unison
x = 2
type MyType = MyType Nat
```

```unison
x = 3
type MyType = MyType Int
```

```ucm
.mergeA> merge .mergeB

  Here's what's changed in the current namespace after the
  merge:
  
  New name conflicts:
  
    1.  type MyType#ig1g2ka7lv
        ↓
    2.  ┌ type MyType#8c6f40i3tj
    3.  └ type MyType#ig1g2ka7lv
    
    4.  MyType.MyType#ig1g2ka7lv#0 : Nat -> MyType#ig1g2ka7lv
        ↓
    5.  ┌ MyType.MyType#8c6f40i3tj#0 : Int -> MyType#8c6f40i3tj
    6.  └ MyType.MyType#ig1g2ka7lv#0 : Nat -> MyType#ig1g2ka7lv
    
    7.  x#dcgdua2lj6 : Nat
        ↓
    8.  ┌ x#dcgdua2lj6 : Nat
    9.  └ x#f3lgjvjqoo : Nat
  
  Updates:
  
    10. patch patch (added 2 updates)
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

  Applying changes from patch...

  I tried to auto-apply the patch, but couldn't because it
  contained contradictory entries.

.mergeA> todo

  ❓
  
  These definitions were edited differently in namespaces that
  have been merged into this one. You'll have to tell me what to
  use as the new definition:
  
    The type 1. #8h7qq3ougl was replaced with
      2. MyType#8c6f40i3tj
      3. MyType#ig1g2ka7lv
    The term 4. #gjmq673r1v was replaced with
      5. x#dcgdua2lj6
      6. x#f3lgjvjqoo
  ❓
  
  The term MyType.MyType has conflicting definitions:
    7. MyType.MyType#8c6f40i3tj#0
    8. MyType.MyType#ig1g2ka7lv#0
  
  Tip: This occurs when merging branches that both independently
       introduce the same name. Use `move.term` or `delete.term`
       to resolve the conflicts.

```
## A named value that appears on the LHS of a patch isn't shown

```unison
foo = 801
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo : Nat

```
```ucm
.lhs> add

  ⍟ I've added these definitions:
  
    foo : Nat

```
```unison
foo = 802
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      foo : Nat

```
```ucm
.lhs> update.old

  ⍟ I've updated these names to your new definition:
  
    foo : Nat

```
```unison
oldfoo = 801
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      oldfoo : Nat

```
```ucm
.lhs> add

  ⍟ I've added these definitions:
  
    oldfoo : Nat

.lhs> view.patch patch

  Edited Terms: 1. oldfoo -> 2. foo
  
  Tip: To remove entries from a patch, use
       delete.term-replacement or delete.type-replacement, as
       appropriate.

.lhs> todo

  ✅
  
  No conflicts or edits in progress.

```
## A type-changing update to one element of a cycle, which doesn't propagate to the other

```unison
even = cases
  0 -> true
  n -> odd (drop 1 n)

odd = cases
  0 -> false
  n -> even (drop 1 n)
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      even : Nat -> Boolean
      odd  : Nat -> Boolean

```
```ucm
.cycle2> add

  ⍟ I've added these definitions:
  
    even : Nat -> Boolean
    odd  : Nat -> Boolean

```
```unison
even = 17
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      even : Nat

```
```ucm
.cycle2> update.old

  ⍟ I've updated these names to your new definition:
  
    even : Nat

```
```ucm
.cycle2> todo

  🚧
  
  The namespace has 1 transitive dependent(s) left to upgrade.
  Your edit frontier is the dependents of these definitions:
  
    #kkohl7ba1e : Nat -> Boolean
  
  I recommend working on them in the following order:
  
  1. odd : Nat -> Boolean
  
  

```

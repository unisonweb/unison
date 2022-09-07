# Test the `todo` command

## Simple type-changing update.

```unison
x = 1
useX = x + 10

structural type MyType = MyType Nat
useMyType = match MyType 1 with
  MyType a -> a + 10
```

Perform a type-changing update so dependents are added to our update frontier.

```unison
x = -1

structural type MyType = MyType Text
```

```ucm
.simple> update

  ⍟ I've updated these names to your new definition:
  
    structural type MyType
    x : Int

.simple> todo

  🚧
  
  The namespace has 2 transitive dependent(s) left to upgrade.
  Your edit frontier is the dependents of these definitions:
  
    structural type MyType#68k40ra7l7
    x#gjmq673r1v : Nat
  
  I recommend working on them in the following order:
  
  1. useMyType : Nat
  2. useX      : Nat
  
  

.> cd .

```
## A merge with conflicting updates.

```unison
x = 1
structural type MyType = MyType
```

Set up two branches with the same starting point.

Update `x` to a different term in each branch.

```unison
x = 2
structural type MyType = MyType Nat
```

```unison
x = 3
structural type MyType = MyType Int
```

```ucm
.mergeA> merge .mergeB

  Here's what's changed in the current namespace after the
  merge:
  
  New name conflicts:
  
    1.  structural type MyType#68k40ra7l7
           
        ↓
    2.  ┌ structural type MyType#68k40ra7l7
             
    3.  └ structural type MyType#eo6rj0lj1b
             
    
    4.  MyType.MyType#68k40ra7l7#0 : Nat -> MyType#68k40ra7l7
        ↓
    5.  ┌ MyType.MyType#68k40ra7l7#0 : Nat -> MyType#68k40ra7l7
    6.  └ MyType.MyType#eo6rj0lj1b#0 : Int -> MyType#eo6rj0lj1b
    
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

  I tried to auto-apply the patch, but couldn't because it
  contained contradictory entries.

.mergeA> todo

  ❓
  
  These definitions were edited differently in namespaces that
  have been merged into this one. You'll have to tell me what to
  use as the new definition:
  
    The type 1. .builtin.Unit was replaced with
      2. MyType#68k40ra7l7
      3. MyType#eo6rj0lj1b
    The term 4. #gjmq673r1v was replaced with
      5. x#dcgdua2lj6
      6. x#f3lgjvjqoo
  ❓
  
  The term MyType.MyType has conflicting definitions:
    7. MyType.MyType#68k40ra7l7#0
    8. MyType.MyType#eo6rj0lj1b#0
  
  Tip: This occurs when merging branches that both independently
       introduce the same name. Use `move.term` or `delete.term`
       to resolve the conflicts.

```
## An update to one element of a cycle, but not the other

```ucm
  ☝️  The namespace .cycle is empty.

.cycle> builtins.mergeio

  Done.

```
```unison
even = cases
  0 -> true
  n -> odd (drop 1 n)

odd = cases
  0 -> false
  n -> even (drop 1 n)
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      even : Nat -> Boolean
      odd  : Nat -> Boolean

```
```ucm
.cycle> add

  ⍟ I've added these definitions:
  
    even : Nat -> Boolean
    odd  : Nat -> Boolean

```
```unison
even = 17
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      even : Nat

```
```ucm
.cycle> update

  ⍟ I've updated these names to your new definition:
  
    even : Nat

.cycle> view odd

  odd : Nat -> Boolean
  odd = cases
    0 -> false
    n -> #kkohl7ba1e (Nat.drop 1 n)

.cycle> view.patch patch

  Edited Terms: 1. even#kkohl7ba1e -> 2. even
  
  Tip: To remove entries from a patch, use
       delete.term-replacement or delete.type-replacement, as
       appropriate.

.cycle> todo

  ✅
  
  No conflicts or edits in progress.

```

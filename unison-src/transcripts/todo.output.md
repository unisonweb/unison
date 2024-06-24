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
.simple> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Now I'm making sure everything typechecks...

  Typechecking failed. I've updated your scratch file with the
  definitions that need fixing. Once the file is compiling, try
  `update` again.

.simple> todo

  ✅
  
  No conflicts or edits in progress.

```
```unison:added-by-ucm scratch.u
useMyType : Nat
useMyType =
  use Nat +
  (MyType a) = MyType 1
  a + 10

useX : Nat
useX =
  use Nat +
  x + 10

x = -1

type MyType = MyType Text
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
.mergeA> merge.old .mergeB

  Here's what's changed in the current namespace after the
  merge:
  
  New name conflicts:
  
    1. type MyType#5287s21ais
       ↓
    2. ┌ type MyType#5287s21ais
    3. └ type MyType#m6mdqhqcr1
    
    4. MyType.MyType#5287s21ais#0 : Nat -> MyType#5287s21ais
       ↓
    5. ┌ MyType.MyType#5287s21ais#0 : Nat -> MyType#5287s21ais
    6. └ MyType.MyType#m6mdqhqcr1#0 : Int -> MyType#m6mdqhqcr1
    
    7. x#dcgdua2lj6 : Nat
       ↓
    8. ┌ x#dcgdua2lj6 : Nat
    9. └ x#f3lgjvjqoo : Nat
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

  Applying changes from patch...

.mergeA> todo

  ❓
  
  The type MyType has conflicting definitions:
    1. MyType#5287s21ais
    2. MyType#m6mdqhqcr1
  
  The term MyType.MyType has conflicting definitions:
    3. MyType.MyType#5287s21ais#0
    4. MyType.MyType#m6mdqhqcr1#0
  The term x has conflicting definitions:
    5. x#dcgdua2lj6
    6. x#f3lgjvjqoo
  
  Tip: This occurs when merging branches that both independently
       introduce the same name. Use `move.type` or `delete.type`
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
.lhs> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

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

.lhs> todo

  ✅
  
  No conflicts or edits in progress.

```
## A type-changing update to one element of a cycle

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
Updating should bring the other half into scope.

```ucm
.cycle2> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Now I'm making sure everything typechecks...

  Typechecking failed. I've updated your scratch file with the
  definitions that need fixing. Once the file is compiling, try
  `update` again.

```
```unison:added-by-ucm scratch.u
odd : Nat -> Boolean
odd = cases
  0 -> false
  n -> even (Nat.drop 1 n)

even = 17
```

We can manually break the cycle:

```unison
odd = 22

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
      odd  : Nat

```
```ucm
.cycle2> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

.cycle2> todo

  ✅
  
  No conflicts or edits in progress.

```

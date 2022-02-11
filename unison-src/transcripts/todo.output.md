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
  
    structural type MyType#d97e0jhkmd
    x#jk19sm5bf8 : Nat
  
  I recommend working on them in the following order:
  
  1. useMyType : Nat
  2. useX      : Nat
  
  

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
  
    1.  structural type MyType#d97e0jhkmd
           
        ↓
    2.  ┌ structural type MyType#d97e0jhkmd
             
    3.  └ structural type MyType#muulibntaq
             
    
    4.  MyType.MyType#d97e0jhkmd#0 : Nat -> MyType#d97e0jhkmd
        ↓
    5.  ┌ MyType.MyType#d97e0jhkmd#0 : Nat -> MyType#d97e0jhkmd
    6.  └ MyType.MyType#muulibntaq#0 : Int -> MyType#muulibntaq
    
    7.  x#0ja1qfpej6 : Nat
        ↓
    8.  ┌ x#0ja1qfpej6 : Nat
    9.  └ x#msp7bv40rv : Nat
  
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
      2. MyType#d97e0jhkmd
      3. MyType#muulibntaq
    The term 4. #jk19sm5bf8 was replaced with
      5. x#0ja1qfpej6
      6. x#msp7bv40rv
  ❓
  
  The term MyType.MyType has conflicting definitions:
    7. MyType.MyType#d97e0jhkmd#0
    8. MyType.MyType#muulibntaq#0
  
  Tip: This occurs when merging branches that both independently
       introduce the same name. Use `move.term` or `delete.term`
       to resolve the conflicts.

```

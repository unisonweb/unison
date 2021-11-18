# delete.namespace.force

```unison
no_dependencies.thing = "no dependents on this term"

dependencies.term1 = 1
dependencies.term2 = 2

dependents.usage1 = dependencies.term1 + dependencies.term2
dependents.usage2 = dependencies.term1 * dependencies.term2
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      dependencies.term1    : Nat
      dependencies.term2    : Nat
      dependents.usage1     : Nat
      dependents.usage2     : Nat
      no_dependencies.thing : Text

```
Deleting a namespace with no external dependencies should succeed.

```ucm
.> delete.namespace no_dependencies

  Removed definitions:
  
    1. thing : Text
  
  Tip: You can use `undo` or `reflog` to undo this change.

```
Deleting a namespace with external dependencies should fail and list all dependents.

```ucm
.> delete.namespace dependencies

  ⚠️
  
  I couldn't delete
  
    1. dependencies.term1 : builtin.Nat
    2. dependencies.term2 : builtin.Nat
    
  
  because it's still being used by these definitions:
  
    1. dependents.usage1 : builtin.Nat
    2. dependents.usage2 : builtin.Nat
    

```

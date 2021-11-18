# delete.namespace.force

```unison
no_dependencies.thing = "no dependents on this term"

dependencies.term1 = 1
dependencies.term2 = 2

dependents.usage1 = dependencies.term1 + dependencies.term2
dependents.usage2 = dependencies.term1 * dependencies.term2
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
Deleting a namespace with external dependencies should succeed when using `delete.namespace.force`

```ucm
.> delete.namespace.force dependencies

  ⚠️
  
  I deleted the following
  
    1. dependencies.term1 : builtin.Nat
    2. dependencies.term2 : builtin.Nat
    
  
  the following now depend on terms which now have no names:
  
    1. dependents.usage1 : builtin.Nat
    2. dependents.usage2 : builtin.Nat
    

  Removed definitions:
  
    1. term1 : Nat
    2. term2 : Nat
  
  Tip: You can use `undo` or `reflog` to undo this change.

```
Deleting the root namespace should require confirmation if not forced.

```ucm
.> delete.namespace .

  ⚠️
  
  Are you sure you want to clear away everything?
  You could use `namespace` to switch to a new namespace instead.

.> delete.namespace .

  Okay, I deleted everything except the history. Use `undo` to
  undo, or `builtins.merge` to restore the absolute basics to
  the current path.

```
Deleting the root namespace shouldn't require confirmation if forced.

```ucm
.> delete.namespace.force .

  Okay, I deleted everything except the history. Use `undo` to
  undo, or `builtins.merge` to restore the absolute basics to
  the current path.

```

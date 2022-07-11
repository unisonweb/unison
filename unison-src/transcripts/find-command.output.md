```unison
foo = 1
lib.foo = 2
lib.bar = 3
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo     : ##Nat
      lib.bar : ##Nat
      lib.foo : ##Nat

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    foo     : ##Nat
    lib.bar : ##Nat
    lib.foo : ##Nat

```
```ucm
.> find foo

  1. foo : ##Nat
  

```
```ucm
.> find bar

  ☝️
  
  I couldn't find matches in this namespace, searching in
  'lib'...

  1. lib.bar : ##Nat
  

```
```ucm
.> find baz

  ☝️
  
  I couldn't find matches in this namespace, searching in
  'lib'...

  😶
  
  No results. Check your spelling, or try using tab completion
  to supply command arguments.
  
  `find.global` can be used to search outside the current
  namespace.

```

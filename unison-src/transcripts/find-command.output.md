```unison
foo = 1
lib.foo = 2
lib.bar = 3
foo.lib.qux = 4
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo         : ##Nat
      foo.lib.qux : ##Nat
      lib.bar     : ##Nat
      lib.foo     : ##Nat

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    foo         : ##Nat
    foo.lib.qux : ##Nat
    lib.bar     : ##Nat
    lib.foo     : ##Nat

```
```ucm
.> find foo

  1. foo : ##Nat
  2. foo.lib.qux : ##Nat
  

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
```ucm
.> find qux

  1. foo.lib.qux : ##Nat
  

```
```ucm
.> find.global nothere

  😶
  
  No results. Check your spelling, or try using tab completion
  to supply command arguments.
  

```

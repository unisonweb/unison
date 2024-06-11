```unison
foo = 1
lib.foo = 2
lib.bar = 3
cat.foo = 4
cat.lib.foo = 5
cat.lib.bar = 6
somewhere.bar = 7
```

```ucm
scratch/main> find foo

  1. cat.foo : Nat
  2. foo : Nat
  

scratch/main> view 1

  cat.foo : Nat
  cat.foo = 4

scratch/main> find.all foo

  1. cat.foo : Nat
  2. cat.lib.foo : Nat
  3. lib.foo : Nat
  4. foo : Nat
  

scratch/main> view 1

  cat.foo : Nat
  cat.foo = 4

```
```ucm
scratch/main> find-in cat foo

  1. foo : Nat
  

scratch/main> view 1

  cat.foo : Nat
  cat.foo = 4

scratch/main> find-in.all cat foo

  1. lib.foo : Nat
  2. foo : Nat
  

scratch/main> view 1

  cat.lib.foo : Nat
  cat.lib.foo = 5

```
```ucm
  ☝️  The namespace .somewhere is empty.

.somewhere> find bar

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
.somewhere> find bar.somewhere> find.global bar
```


🛑

The transcript failed due to an error in the stanza above. The error is:


  😶
  
  No results. Check your spelling, or try using tab completion
  to supply command arguments.
  
  `find.global` can be used to search outside the current
  namespace.


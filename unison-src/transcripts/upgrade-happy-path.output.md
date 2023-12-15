```unison
lib.old.foo = 17
lib.new.foo = 18
thingy = lib.old.foo + 10
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      lib.new.foo : Nat
      lib.old.foo : Nat
      thingy      : Nat

```
```ucm
proj/main> add

  ⍟ I've added these definitions:
  
    lib.new.foo : Nat
    lib.old.foo : Nat
    thingy      : Nat

proj/main> upgrade old new

  I upgraded old to new, and removed old.

proj/main> ls lib

  1. builtin/ (453 terms, 70 types)
  2. new/     (1 term)

proj/main> view thingy

  thingy : Nat
  thingy =
    use Nat +
    foo + 10

```

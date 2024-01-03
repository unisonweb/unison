```unison
lib.old.foo = 17
lib.new.foo = +18
thingy = lib.old.foo + 10
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      lib.new.foo : Int
      lib.old.foo : Nat
      thingy      : Nat

```
```ucm
proj/main> add

  ⍟ I've added these definitions:
  
    lib.new.foo : Int
    lib.old.foo : Nat
    thingy      : Nat

```
```ucm
proj/main> upgrade old new

  thingy : Nat
  thingy =
    use Nat +
    foo + 10

  I couldn't automatically upgrade old to new.
  
  I added the problematic definitions to the top of scratch.u.

```

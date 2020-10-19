Regression test for https://github.com/unisonweb/unison/issues/763

```unison
(+-+) : Nat -> Nat -> Nat
(+-+) x y = x * y
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      +-+ : Nat -> Nat -> Nat

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    +-+ : Nat -> Nat -> Nat

.> move.term +-+ boppitybeep

  Done.

.> move.term boppitybeep +-+

  Done.

```

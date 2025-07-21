Regression test for https://github.com/unisonweb/unison/issues/763

``` ucm :hide
scratch/main> builtins.merge
```

``` unison
(+-+) : Nat -> Nat -> Nat
(+-+) x y = x * y
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + +-+ : Nat -> Nat -> Nat

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> move.term +-+ boppitybeep

  Done.

scratch/main> move.term boppitybeep +-+

  Done.
```

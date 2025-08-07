Regression test for https://github.com/unisonweb/unison/issues/763

``` ucm :hide
> builtins.merge
```

``` unison
(+-+) : Nat -> Nat -> Nat
(+-+) x y = x * y
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + (+-+) : Nat -> Nat -> Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

> move.term +-+ boppitybeep

  Done.

> move.term boppitybeep +-+

  Done.
```

# `docs` for definitions that are only in the scratch file

`docs` shows the type signature (or the heading of a type) and then the documentation, so you can preview the docs
while you are still writing them.

``` unison :hide
{{ Adds one to a number. }}
inc : Nat -> Nat
inc n = n + 1

{{ A pair of things. }}
type Pair a b = Pair a b
```

``` ucm
scratch/main> load

  Loading changes detected in scratch.u.

  + type Pair a b

  + inc      : Nat -> Nat
  + inc.doc  : Doc
  + Pair.doc : Doc

  Run `update` to apply these changes to your codebase.

scratch/main> docs inc

  inc : Nat -> Nat

  Adds one to a number.

scratch/main> docs Pair

  type Pair a b

  A pair of things.
```

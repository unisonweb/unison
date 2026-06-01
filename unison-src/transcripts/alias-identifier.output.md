# `alias` as an identifier

`alias` is contextual — it's only treated specially after `type`. Outside
that position it remains a normal identifier.

``` ucm :hide
> builtins.mergeio
```

``` unison
alias : Nat -> Nat
alias n = n + 1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + alias : Nat -> Nat
  - g : Endo Nat

  (and 137 unchanged types and 709 unchanged terms)

  + (added), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  I fast-forward merged scratch/update-main into scratch/main.

  Done.

> view alias

  alias : Nat -> Nat
  alias n =
    use Nat +
    n + 1
```

# Update on conflict

Conflicted definitions prevent `update` from succeeding.

``` ucm :hide
> builtins.merge lib.builtins
```

``` unison
x = 1
temp = 2
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + temp : Nat
  + x    : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

> debug.alias.term.force temp x

  Done.

> delete.term temp

  Done.
```

``` unison
x = 3
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ x : Nat

  ~ (modified)

  Run `update` to apply these changes to your codebase.
```

``` ucm :error
> update

  This branch has more than one term with the name `x`. Please
  delete or rename all but one of them, then try the update
  again.
```

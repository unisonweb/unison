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

> delete.term.force temp

  I deleted these definitions:

    term temp

  Tip: You can use `undo` or use a hash from `reflog` to undo
       this change.
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

  Sorry, I can't do that right now, because there's more than
  one term with the name `x`. Please rename all but one of them,
  then try again.
```

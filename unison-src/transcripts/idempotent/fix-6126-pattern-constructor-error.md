Test for https://github.com/unisonweb/unison/issues/6126
When a pattern constructor receives a wrong argument type, the error message
should say "The Nth argument to `Constructor`" instead of "The Nth argument to `function`".

``` ucm :hide
> builtins.merge
```

``` unison
type X = X
type Y k = Y X
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type X
  + type Y k

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

This should show "The 1st argument to `Y`" not "The 1st argument to `b`":

``` unison :error
type X = X
type Y k = Y X

a =
  b cases
    Y (3,4) -> "c"

b : (a -> b) -> c
b = todo "whatever"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  The 1st argument to `Y`

            has type:  Tuple
      but I expected:  X

      8 |     Y (3,4) -> "c"
```

See [this ticket](https://github.com/unisonweb/unison/issues/873); the point being, this shouldn't crash the runtime. :)

``` ucm :hide
> builtins.merge
```

``` unison
(-) = builtin.Nat.sub
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + (-) : Nat -> Nat -> Int

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
baz x = x - 1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + baz : Nat -> Int

  Run `update` to apply these changes to your codebase.
```

``` ucm :hide
> builtins.merge
```

``` unison
bonk : forall a. a -> a
bonk x =
  zonk : forall a. a -> a
  zonk z = z
  honk : a
  honk = x
  x
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + bonk : a -> a

  Run `update` to apply these changes to your codebase.
```

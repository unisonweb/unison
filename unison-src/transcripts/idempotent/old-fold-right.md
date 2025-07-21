``` ucm :hide
scratch/main> builtins.merge
```

``` unison
oldRight: (b ->{e} a ->{e} b) -> [a] ->{e} [b]
oldRight f la = bug "out"

pecan: '{} [Text]
pecan = 'let
  la = [1, 2, 3]
  f: Text -> Nat -> Text
  f = bug "out"

  oldRight f la
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + oldRight : (b ->{e} a ->{e} b) -> [a] ->{e} [b]
  + pecan    : '[Text]

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

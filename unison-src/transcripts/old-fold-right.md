```ucm:hide
.> builtins.merge
```

```unison
oldRight: (b ->{e} a ->{e} b) -> [a] ->{e} [b]
oldRight f la = bug "out"

pecan: '{} [Text]
pecan = 'let
  la = [1, 2, 3]
  f: Text -> Nat -> Text
  f = bug "out"

  oldRight f la
```


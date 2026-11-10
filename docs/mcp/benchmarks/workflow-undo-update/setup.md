The first version of `Cfg.parse`.

``` unison
Cfg.parse : Text -> Optional Nat
Cfg.parse t = Nat.fromText t

test> Cfg.parse.tests.example = test.verify do
  ensureEqual (Cfg.parse "8080") (Some 8080)
  ensureEqual (Cfg.parse "abc") None
  ensureEqual (Cfg.parse "") None
```

``` ucm
scratch/main> update
```

A change to `Cfg.parse`, tests updated to match.

``` unison
Cfg.parse : Text -> Optional Nat
Cfg.parse t = Some (Optional.getOrElse 0 (Nat.fromText t))

test> Cfg.parse.tests.example = test.verify do
  ensureEqual (Cfg.parse "8080") (Some 8080)
  ensureEqual (Cfg.parse "abc") (Some 0)
  ensureEqual (Cfg.parse "") (Some 0)
```

``` ucm
scratch/main> update
```

Later work that uses `Cfg.parse`.

``` unison
Cfg.render : Text -> Text
Cfg.render t = match Cfg.parse t with
  Some n -> "port " ++ Nat.toText n
  None -> "invalid"

test> Cfg.render.tests.example = test.verify do
  ensureEqual (Cfg.render "8080") "port 8080"
```

``` ucm
scratch/main> update
```

More later work, unrelated to `Cfg.parse`.

``` unison
Cfg.defaultPort : Nat
Cfg.defaultPort = 8080

test> Cfg.defaultPort.tests.example = test.verify do
  ensureEqual Cfg.defaultPort 8080
```

``` ucm
scratch/main> update
```

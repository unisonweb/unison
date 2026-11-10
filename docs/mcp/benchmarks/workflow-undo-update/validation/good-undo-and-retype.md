<!-- expected: all judges PASS. Another correct way: look at the later work, undo the three updates back to the first version of Cfg.parse, and retype the later work. -->
``` ucm
scratch/main> view Cfg.render Cfg.render.tests.example Cfg.defaultPort Cfg.defaultPort.tests.example
scratch/main> undo
scratch/main> undo
scratch/main> undo
scratch/main> view Cfg.parse Cfg.parse.tests.example
```

``` unison
Cfg.render : Text -> Text
Cfg.render t = match Cfg.parse t with
  Some n -> "port " ++ Nat.toText n
  None -> "invalid"

test> Cfg.render.tests.example = test.verify do
  ensureEqual (Cfg.render "8080") "port 8080"

Cfg.defaultPort : Nat
Cfg.defaultPort = 8080

test> Cfg.defaultPort.tests.example = test.verify do
  ensureEqual Cfg.defaultPort 8080
```

``` ucm
scratch/main> update
scratch/main> test
```

<!-- expected: behavior FAIL and outcome FAIL (parse also rejects zero, which the first version never did, so it is not how it was before; its test passes) -->
``` unison
Cfg.parse : Text -> Optional Nat
Cfg.parse t = Optional.filter (n -> n > 0) (Nat.fromText t)

test> Cfg.parse.tests.example = test.verify do
  ensureEqual (Cfg.parse "8080") (Some 8080)
  ensureEqual (Cfg.parse "abc") None
  ensureEqual (Cfg.parse "") None
```

``` ucm
scratch/main> update
scratch/main> test
```

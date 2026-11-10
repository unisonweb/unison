<!-- expected: outcome FAIL and behavior FAIL (parse was put back by hand but its test still expects Some 0 for "abc", and the stale test was then deleted or left failing: here the test keeps the wrong expectation, so the codebase has parse restored and a failing test) -->
``` unison
Cfg.parse : Text -> Optional Nat
Cfg.parse t = Nat.fromText t
```

``` ucm
scratch/main> update
```

``` ucm :error
scratch/main> test
```

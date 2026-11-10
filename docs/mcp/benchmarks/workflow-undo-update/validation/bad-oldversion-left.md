<!-- expected: outcome FAIL and no-collateral FAIL (Cfg.parse and its test are restored and the later work is kept, but the `oldversion` subnamespace made to look at the old version is still in the branch, with a whole copy of lib in it) -->
``` ucm
scratch/main> reflog
scratch/main> fork #j5ae1dgiq1 oldversion
scratch/main> view oldversion.Cfg.parse oldversion.Cfg.parse.tests.example
scratch/main> delete.term Cfg.parse.tests.example
scratch/main> move oldversion.Cfg.parse.tests.example Cfg.parse.tests.example
```

``` unison
Cfg.parse : Text -> Optional Nat
Cfg.parse t = Nat.fromText t
```

``` ucm
scratch/main> update
scratch/main> test
```

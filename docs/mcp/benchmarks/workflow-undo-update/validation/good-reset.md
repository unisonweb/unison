<!-- The older route: reset to the state before the change to view it, come back with reset to the newest hash, and restore both by hand. The hashes are the ones this setup makes (printed by reflog in a fresh copy); they are stable for a given setup.md and base library. -->
``` ucm
scratch/main> reflog
scratch/main> reset #j5ae1dgiq1
scratch/main> view Cfg.parse Cfg.parse.tests.example
scratch/main> reset #smeu0lei4p
```

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
scratch/main> test
```

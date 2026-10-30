<!-- Finds the state before the change with reflog, looks at the old Cfg.parse and its test there, comes back to the latest state, and restores both. The hashes are the ones this setup makes (printed by reflog in a fresh copy); they are stable for a given setup.md and base library. -->
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

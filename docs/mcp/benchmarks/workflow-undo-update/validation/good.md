<!-- Finds the state before the change with reflog, forks it into a subnamespace `oldversion` of the current branch, looks at the old Cfg.parse and its test there, moves the old test out, retypes Cfg.parse (a name that exists in the branch cannot be the target of a move), and deletes `oldversion`. The hashes are the ones this setup makes (printed by reflog in a fresh copy); they are stable for a given setup.md and base library. -->
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
scratch/main> delete.namespace oldversion
scratch/main> test
```

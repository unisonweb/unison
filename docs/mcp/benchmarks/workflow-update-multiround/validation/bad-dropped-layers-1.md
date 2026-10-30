<!-- expected: outcome FAIL (parts 1 and 2 together), no-collateral FAIL -->
Part 1 of 2, the same as `good-via-conflict-1.md`. In part 2 the file the worker loads has only the first two layers (it forgot `Server.status`, `Server.report` and their tests, which were in the file too), and `update` removes what the file no longer has.

``` unison
unique type Config = { host : Text, retries : Nat, port : Nat }

Config.parse : Text -> Nat -> Config
Config.parse host port = Config host 3 port
```

``` ucm :error
scratch/main> update
```

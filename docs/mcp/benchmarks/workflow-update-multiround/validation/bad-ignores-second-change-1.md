<!-- expected: outcome FAIL, behavior FAIL (parts 1 and 2 together) -->
Part 1 of 2, the same as `good-via-conflict-1.md`. In part 2 the worker fixes the connection layer, loads it and updates: that succeeds (the retry layer is untouched), the worker is back on `main` with a clean `todo` and passing tests, but never made the second change.

``` unison
unique type Config = { host : Text, retries : Nat, port : Nat }

Config.parse : Text -> Nat -> Config
Config.parse host port = Config host 3 port
```

``` ucm :error
scratch/main> update
```

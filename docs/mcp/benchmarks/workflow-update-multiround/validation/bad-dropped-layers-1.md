<!-- expected: outcome FAIL (parts 1 to 3 together), no-collateral FAIL -->
Part 1 of 3, the same as `good-via-conflict-1.md`. Part 2 is `good-via-conflict-2.md` itself. In part 3 the file the worker loads has the connection layer and the first two definitions of the retry layer, but forgot `Job.total`, `Job.within` and their tests, which were in the second file; `update` removes what the file no longer has, and nothing complains.

``` unison
unique type Config = { host : Text, retries : Nat, port : Nat }

Config.parse : Text -> Nat -> Config
Config.parse host port = Config host 3 port
```

``` ucm :error
scratch/main> update
```

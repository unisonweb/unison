<!-- expected: outcome FAIL -->
The worker did the first change, ran `update`, and stopped when it failed, or went on until the second failure and stopped there (run `good-via-conflict-1.md` and then `good-via-conflict-2.md`; the state is the same): UCM leaves it on `update-main` and nothing is saved into `main`. This file is the first case.

``` unison
unique type Config = { host : Text, retries : Nat, port : Nat }

Config.parse : Text -> Nat -> Config
Config.parse host port = Config host 3 port
```

``` ucm :error
scratch/main> update
```

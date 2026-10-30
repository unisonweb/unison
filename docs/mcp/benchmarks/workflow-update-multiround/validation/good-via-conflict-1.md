Part 1 of 3, the natural route. The worker starts with the first change, the type and `Config.parse`, and runs `update`: it can't finish, UCM leaves the codebase on `update-main` with a file that holds the new type, the new `parse`, and every definition and test of the connection layer that no longer typechecks. The retry layer is not in the file, since nothing in it depends on `Config`. A transcript can't read that file, so part 2 (`good-via-conflict-2.md`, run on the codebase this leaves, from this directory) loads what the worker would have in the file after fixing the connection layer and adding the new `Backoff.delay`. Worker transcripts: `update` (fails), `load`, `update` (fails again), `load`, `update`.

``` unison
unique type Config = { host : Text, retries : Nat, port : Nat }

Config.parse : Text -> Nat -> Config
Config.parse host port = Config host 3 port
```

``` ucm :error
scratch/main> update
```

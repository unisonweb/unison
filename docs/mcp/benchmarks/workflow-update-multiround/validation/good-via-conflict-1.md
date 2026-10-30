Part 1 of 2. The worker changes the type and the first function and runs `update`: it can't finish, UCM leaves the codebase on `update-main` with a file that holds the new type, the new `parse`, and every other definition and test that no longer typechecks, all four layers at once. A transcript can't read that file, so part 2 (`good-via-conflict-2.md`, run on the codebase this leaves, from this directory) loads `fixed.u`, which is what the worker would have in the file after fixing it. Worker transcripts: `update` (fails), `load`, `update`; each extra `load` is a round of fixing errors in the file.

``` unison
unique type Config = { host : Text, retries : Nat, port : Nat }

Config.parse : Text -> Nat -> Config
Config.parse host port = Config host 3 port
```

``` ucm :error
scratch/main> update
```

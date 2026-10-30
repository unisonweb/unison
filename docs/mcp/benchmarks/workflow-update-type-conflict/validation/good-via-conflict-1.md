Part 1 of 2. The worker changes the type alone, and `update` can't finish: it leaves the codebase on `update-main`, with a file of the definitions to fix. A transcript can't read that file, so part 2 (`good-via-conflict-2.md`, run on the codebase this leaves, from this directory) loads `fixed.u`, which has what the worker would write in the file: the new type, then all the definitions and tests, fixed. In the worker's transcripts this takes `update` (fails), `load`, `update`: one round.

``` unison
unique type Account = { owner : Text, balance : Nat, currency : Text }
```

``` ucm :error
scratch/main> update
```

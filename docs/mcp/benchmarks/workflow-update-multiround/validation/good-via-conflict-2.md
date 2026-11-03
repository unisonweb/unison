Part 2 of 3: run on the codebase left by `good-via-conflict-1.md`, from the `validation` directory. `round-2.u` is the file after the worker fixed the connection layer and added the new `Backoff.delay`. The second `update` fails too: UCM now lists the dependents of `Backoff.delay` (`Job.wait`, `Job.total`, `Job.within` and their tests) in a new file, on top of what the worker already fixed.

``` ucm
scratch/update-main> load round-2.u
```

``` ucm :error
scratch/update-main> update
```

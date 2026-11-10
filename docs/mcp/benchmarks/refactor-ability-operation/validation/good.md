<!-- The update conflict flow. Adding `warn` to the ability makes every handler (their `case`s are not exhaustive any more)
     and everything that uses `Journal` stop typechecking, so the first `update` can't finish: it leaves the temporary
     branch `update-main` and a file (named like `update-main-<hash>.u`) with the new ability and all of those definitions.
     `good.u` is that file after editing by hand: a `warn` case in each handler, `Inventory.audit` using `warn`, and tests.
     A worker edits the file in place; here a copy stands in. Run with the directory of this file as the working directory. -->

``` unison
ability Journal where
  entry : Text ->{Journal} ()
  warn : Text ->{Journal} ()
```

``` ucm :error
scratch/main> update
```

``` ucm
scratch/update-main> load good.u
scratch/update-main> update
scratch/main> test
scratch/main> branches
scratch/main> todo
```

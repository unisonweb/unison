<!-- Same end state by hand, through the update conflict flow. The first `update` can't finish: it leaves the temporary
     branch `update-main` and a file with the new `Log.line` and every dependent that no longer typechecks (the file is
     named like `update-main-<hash>.u`). `good-manual.u` is that file after editing by hand; a worker edits the file in
     place, here a copy stands in. Run with the directory of this file as the working directory. -->

``` unison
Log.line : Nat -> Text -> Text -> Text
Log.line code level message =
  use Text ++
  "[" ++ level ++ " " ++ Nat.toText code ++ "] " ++ message
```

``` ucm :error
scratch/main> update
```

``` ucm
scratch/update-main> load good-manual.u
scratch/update-main> update
scratch/main> test
scratch/main> branches
scratch/main> todo
```

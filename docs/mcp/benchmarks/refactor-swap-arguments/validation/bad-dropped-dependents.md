<!-- expected: outcome FAIL -->
<!-- Finishes the update conflict flow by deleting the doc of `Log.line` and its test from the file
     (`bad-dropped-dependents.u`, a stand-in for the edited `update-main-<hash>.u`), which deletes them from the branch.
     Run with the directory of this file as the working directory. -->

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
scratch/update-main> load bad-dropped-dependents.u
scratch/update-main> update
scratch/main> test
```

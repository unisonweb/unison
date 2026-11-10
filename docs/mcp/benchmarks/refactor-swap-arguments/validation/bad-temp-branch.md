<!-- expected: outcome FAIL -->
<!-- Stops after the first update could not finish: the worker is left on update-main. -->

``` unison
Log.line : Nat -> Text -> Text -> Text
Log.line code level message =
  use Text ++
  "[" ++ level ++ " " ++ Nat.toText code ++ "] " ++ message
```

``` ucm :error
scratch/main> update
```


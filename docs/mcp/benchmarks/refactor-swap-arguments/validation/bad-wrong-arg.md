<!-- expected: outcome FAIL, behavior FAIL -->
<!-- `Log.warn` was fixed with the level and the message in the wrong places: `Log.line code message "WARN"` typechecks
     (Nat, Text, Text) but gives "[careful 3] WARN" for `Log.warn 3 "careful"`. `Log.tests.partial` catches it. Run with the directory of
     this file as the working directory. -->

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
scratch/update-main> load bad-wrong-arg.u
scratch/update-main> update
```

``` ucm :error
scratch/main> test
```

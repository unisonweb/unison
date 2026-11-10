<!-- expected: behavior FAIL, outcome FAIL -->
<!-- Handlers all extended, but `Inventory.audit` still uses `entry` (and its test was left expecting the old text). Run with the directory of this file as the working directory. -->

``` unison
ability Journal where
  entry : Text ->{Journal} ()
  warn : Text ->{Journal} ()
```

``` ucm :error
scratch/main> update
```

``` ucm
scratch/update-main> load bad-audit-unchanged.u
scratch/update-main> update
scratch/main> test
```

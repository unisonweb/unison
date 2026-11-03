<!-- expected: outcome FAIL -->
<!-- Stops after the first update could not finish: the worker is left on update-main. -->

``` unison
ability Journal where
  entry : Text ->{Journal} ()
  warn : Text ->{Journal} ()
```

``` ucm :error
scratch/main> update
```

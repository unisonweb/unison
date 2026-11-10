<!-- expected: behavior FAIL -->
<!-- The collecting and printing handlers handle `warn` but forget the `WARN: ` prefix (the agent wrote its own test to match). Run with the directory of this file as the working directory. -->

``` unison
ability Journal where
  entry : Text ->{Journal} ()
  warn : Text ->{Journal} ()
```

``` ucm :error
scratch/main> update
```

``` ucm
scratch/update-main> load bad-no-prefix.u
scratch/update-main> update
scratch/main> test
```

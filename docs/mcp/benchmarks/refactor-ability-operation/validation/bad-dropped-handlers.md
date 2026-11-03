<!-- expected: outcome FAIL -->
<!-- The conflict flow was finished by deleting the printing and the ignoring handler from the file (which deletes them from the branch) instead of extending them. Run with the directory of this file as the working directory. -->

``` unison
ability Journal where
  entry : Text ->{Journal} ()
  warn : Text ->{Journal} ()
```

``` ucm :error
scratch/main> update
```

``` ucm
scratch/update-main> load bad-dropped-handlers.u
scratch/update-main> update
scratch/main> test
```

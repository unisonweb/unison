<!-- expected: outcome FAIL -->
The worker changes the type, sees `update` fail, and stops there: the codebase is left on the temporary branch `update-main`, `main` unchanged, with the type and every dependent unsaved.

``` unison
unique type Account = { owner : Text, balance : Nat, currency : Text }
```

``` ucm :error
scratch/main> update
```

<!-- expected: outcome FAIL, no-collateral FAIL -->
Part 1 of 2 (the same as `good-via-conflict-1.md`). In part 2 the file the worker loads has lost `Account.audit`, `Account.totalBalance` and their tests, as when a worker writes only the definitions that failed first and leaves the rest out of the file: `update` removes what the file no longer has.

``` unison
unique type Account = { owner : Text, balance : Nat, currency : Text }
```

``` ucm :error
scratch/main> update
```

<!-- expected: outcome FAIL (parts 1 and 2 together), behavior FAIL -->
Part 1 of 2, the same as `good-via-conflict-1.md`. In part 2 the file the worker loads writes `20` into `first` and `count` instead of taking the size as an argument: `Page.size` is gone and the old tests pass unchanged, but nothing takes the page size.

``` ucm :error
scratch/main> delete.term Page.size
```

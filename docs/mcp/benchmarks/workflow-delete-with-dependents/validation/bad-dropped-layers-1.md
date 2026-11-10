<!-- expected: outcome FAIL (parts 1 and 2 together), no-collateral FAIL -->
Part 1 of 2, the same as `good-via-conflict-1.md`. In part 2 the file the worker loads fixes only `first` and `count` (the two that use `Page.size`) and their tests, and leaves `Page.summary`, `Page.report` and their tests out; `update` removes what the file no longer has.

``` ucm :error
scratch/main> delete.term Page.size
```

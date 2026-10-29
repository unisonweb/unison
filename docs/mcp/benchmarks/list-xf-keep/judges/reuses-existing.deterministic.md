# Does `List.xfKeep` reuse `List.filterMap`?

Run by `judge.sh`. This measures whether the worker looked for an existing function before implementing one: the pass is `List.xfKeep = List.filterMap`. Both functions have the same hash when `List.xfKeep` is just another name for `List.filterMap`.

``` unison
test> judge.xfKeep.isExisting = test.verify do
  ensure (crypto.hash Sha2_256 List.xfKeep == crypto.hash Sha2_256 List.filterMap)
```

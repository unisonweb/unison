# Does `List.sift` reuse `List.filter`?

Run by `judge.sh`. This measures whether the worker looked for an existing function before implementing one: the pass is `List.sift = List.filter`. Both functions have the same hash when `List.sift` is just another name for `List.filter`.

``` unison
test> judge.sift.isExisting = test.verify do
  ensure (crypto.hash Sha2_256 List.sift == crypto.hash Sha2_256 List.filter)
```

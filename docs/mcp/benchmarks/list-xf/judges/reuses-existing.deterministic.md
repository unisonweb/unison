# Does `List.xf` reuse `List.map`?

Run by `judge.sh`. This measures whether the worker looked for an existing function before implementing one: the pass is `List.xf = List.map`. Both functions have the same hash when `List.xf` is just another name for `List.map`.

``` unison
test> judge.xf.isExisting = test.verify do
  ensure (crypto.hash Sha2_256 List.xf == crypto.hash Sha2_256 List.map)
```

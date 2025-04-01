
Tests related to automatic replacement of unison types/functions by
builtins.

``` unison
theMap = Bin 2 1 2 (Bin 1 0 1 Tip Tip) (Bin 1 3 4 Tip Tip)

testIt = do
  v = value theMap
  match load v with
    Right m ->
      match Map.get 1 m with
        Some 2 -> Ok "Passed"
        _ -> Fail "wrong lookup value"
    _ -> Fail "could not load value"

mapTests = do [!testIt]

> Map.get 1
```

``` ucm
scratch/main> add
scratch/main> io.test mapTests
```

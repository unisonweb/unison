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

```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      mapTests : '{IO} [Result]
      testIt   : '{IO} Result
      theMap   : Map Nat Nat
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    mapTests : '{IO} [Result]
    testIt   : '{IO} Result
    theMap   : Map Nat Nat

scratch/main> io.test mapTests

    New test results:

    1. mapTests   ◉ Passed

  ✅ 1 test(s) passing

  Tip: Use view 1 to view the source of a test.
```

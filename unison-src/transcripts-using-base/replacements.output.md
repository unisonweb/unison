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

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `update`, here's how your codebase would change:

    ⍟ These new definitions are ok to `update`:
    
      mapTests : '{IO} [Result]
      testIt   : '{IO} Result
      theMap   : Map Nat Nat

  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

    14 | > Map.get 1
           ⧩
           cases
             Tip -> None
             Bin _ kx x l r ->
               match compare 1 kx with
                 -1 -> Map.get 1 l
                 +1 -> Map.get 1 r
                 +0 -> Some x
                 _ ->
                   bug
                     "impossible: Universal.compare returns {-1,0,+1}"
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> io.test mapTests

    New test results:

    1. mapTests   ◉ Passed

  ✅ 1 test(s) passing

  Tip: Use view 1 to view the source of a test.
```

<!-- expected: outcome FAIL (summary and its test lost: the conflict file's dependents were left out of the loaded block) -->
``` ucm :error
scratch/alice> merge /bob
```

``` unison resolved.u
Order.total : Int -> Int
Order.total subtotal = subtotal - +10 + +5

test> Order.total.tests.example = test.verify do
  ensureEqual (Order.total +100) +95
```

``` ucm
scratch/merge-bob-into-alice> load resolved.u
scratch/merge-bob-into-alice> update
scratch/alice> test
```

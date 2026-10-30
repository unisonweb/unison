<!-- expected: outcome FAIL -->
``` ucm :error
scratch/alice> merge /bob
```

``` unison resolved.u
Order.total : Int -> Int
Order.total subtotal = subtotal - +10 + +5

Order.summary : Int -> Text
Order.summary subtotal = "Total: " ++ Int.toText (Order.total subtotal)

test> Order.total.tests.example = test.verify do
  ensureEqual (Order.total +100) +95

test> Order.summary.tests.example = test.verify do
  ensureEqual (Order.summary +100) "Total: 95"
```

``` ucm
scratch/merge-bob-into-alice> load resolved.u
```

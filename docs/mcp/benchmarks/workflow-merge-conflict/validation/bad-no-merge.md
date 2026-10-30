<!-- expected: approach PASS (outcome and behavior PASS, but bob was never merged: the same code typed on alice) -->
``` unison
Order.total : Int -> Int
Order.total subtotal = subtotal + +5 - +10

Order.summary : Int -> Text
Order.summary subtotal = "Total: " ++ Int.toText (Order.total subtotal)

test> Order.total.tests.example = test.verify do
  ensureEqual (Order.total +100) +95

test> Order.summary.tests.example = test.verify do
  ensureEqual (Order.summary +100) "Total: 95"
```

``` ucm
scratch/alice> update
scratch/alice> test
```

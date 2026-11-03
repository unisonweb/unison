<!-- expected: outcome FAIL -->
``` ucm :error
scratch/alice> merge /bob
```

``` unison resolved.u
Order.total : Nat -> Nat
Order.total subtotal = subtotal - 10 + 5

Order.summary : Nat -> Text
Order.summary subtotal = "Total: " ++ Nat.toText (Order.total subtotal)

test> Order.total.tests.example = test.verify do
  ensureEqual (Order.total 100) 95

test> Order.summary.tests.example = test.verify do
  ensureEqual (Order.summary 100) "Total: 95"
```

``` ucm
scratch/merge-bob-into-alice> load resolved.u
```

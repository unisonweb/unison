<!-- expected: approach PASS (outcome and behavior PASS, but bob was never merged: the same code typed on alice) -->
``` unison
Order.total : Nat -> Nat
Order.total subtotal = subtotal + 5 - 10

Order.summary : Nat -> Text
Order.summary subtotal = "Total: " ++ Nat.toText (Order.total subtotal)

test> Order.total.tests.example = test.verify do
  ensureEqual (Order.total 100) 95

test> Order.summary.tests.example = test.verify do
  ensureEqual (Order.summary 100) "Total: 95"
```

``` ucm
scratch/alice> update
scratch/alice> test
```

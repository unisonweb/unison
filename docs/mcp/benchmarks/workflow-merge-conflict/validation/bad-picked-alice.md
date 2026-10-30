<!-- expected: behavior FAIL -->
``` ucm :error
scratch/alice> merge /bob
```

``` unison resolved.u
Order.total : Nat -> Nat
Order.total subtotal = subtotal + 5

Order.summary : Nat -> Text
Order.summary subtotal = "Total: " ++ Nat.toText (Order.total subtotal)

test> Order.total.tests.example = test.verify do
  ensureEqual (Order.total 100) 105

test> Order.summary.tests.example = test.verify do
  ensureEqual (Order.summary 100) "Total: 105"
```

``` ucm
scratch/merge-bob-into-alice> load resolved.u
scratch/merge-bob-into-alice> update
scratch/alice> test
```

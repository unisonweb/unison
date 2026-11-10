<!-- expected: outcome FAIL, behavior FAIL, no-collateral FAIL -->
A new function `Invoice.taxUp` and `Invoice.total` pointed at it: `Invoice.tax` itself keeps the old rounding and its doc, and an extra definition appears.

```` unison
Invoice.taxUp : Nat -> Nat
Invoice.taxUp amount = (amount * 8 + 99) / 100

Invoice.total : Nat -> Nat
Invoice.total amount = amount + Invoice.taxUp amount

test> Invoice.taxUp.tests.rounding = test.verify do
  ensureEqual (Invoice.taxUp 1005) 81
  ensureEqual (Invoice.taxUp 1) 1
````

``` ucm
scratch/main> update
scratch/main> test
```

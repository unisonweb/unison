<!-- expected: outcome FAIL (behavior PASS) -->
The function is right, saved and tested, but its documentation still says "rounded down".

```` unison
Invoice.tax : Nat -> Nat
Invoice.tax amount = (amount * 8 + 99) / 100

test> Invoice.tax.tests.rounding = test.verify do
  ensureEqual (Invoice.tax 1005) 81
  ensureEqual (Invoice.tax 1) 1
````

``` ucm
scratch/main> update
scratch/main> test
```

The tax code of a small invoicing library: `Invoice.tax` is 8% of an amount in cents, rounded down, and two functions depend on it, each with a test and `Invoice.tax` has a doc.

```` unison
Invoice.tax.doc = {{
{{ docExample 1 (do (amount -> Invoice.tax amount)) }} is the tax on `amount`, in cents: 8% of the amount, rounded down to a whole cent.
}}

Invoice.tax : Nat -> Nat
Invoice.tax amount = amount * 8 / 100

Invoice.total : Nat -> Nat
Invoice.total amount = amount + Invoice.tax amount

Invoice.describe : Nat -> Text
Invoice.describe amount = "total: " ++ Nat.toText (Invoice.total amount) ++ " cents"

test> Invoice.tax.tests.example = test.verify do
  ensureEqual (Invoice.tax 1000) 80
  ensureEqual (Invoice.tax 2500) 200
  ensureEqual (Invoice.tax 0) 0

test> Invoice.total.tests.example = test.verify do
  ensureEqual (Invoice.total 1000) 1080
  ensureEqual (Invoice.total 100) 108

test> Invoice.describe.tests.example = test.verify do
  ensureEqual (Invoice.describe 1000) "total: 1080 cents"
````

``` ucm
scratch/main> add
scratch/main> test
```

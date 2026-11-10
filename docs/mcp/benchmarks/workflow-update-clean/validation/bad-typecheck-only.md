<!-- expected: outcome FAIL, behavior FAIL -->
The change is typechecked but never saved: the codebase still has the old `Invoice.tax`.

```` unison
Invoice.tax.doc = {{
{{ docExample 1 (do (amount -> Invoice.tax amount)) }} is the tax on `amount`, in cents: 8% of the amount, rounded up to the next whole cent.
}}

Invoice.tax : Nat -> Nat
Invoice.tax amount = (amount * 8 + 99) / 100

test> Invoice.tax.tests.rounding = test.verify do
  ensureEqual (Invoice.tax 1005) 81
  ensureEqual (Invoice.tax 1) 1
````

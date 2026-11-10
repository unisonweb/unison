<!-- expected: outcome FAIL, no-collateral FAIL -->
The worker avoids the conflict by making a second type, `Account2`, with the new field and new functions next to the old ones. `Account` and everything that uses it are left as they were.

``` unison
unique type Account2 = { owner : Text, balance : Nat, currency : Text }

Account2.open : Text -> Nat -> Account2
Account2.open owner balance = Account2 owner balance "USD"

Account2.deposit : Nat -> Account2 -> Account2
Account2.deposit n a = Account2 (Account2.owner a) (Account2.balance a + n) (Account2.currency a)

Account2.describe : Account2 -> Text
Account2.describe a = Account2.owner a ++ ": " ++ Nat.toText (Account2.balance a) ++ " " ++ Account2.currency a

test> Account2.describe.tests.example = test.verify do
  ensureEqual (Account2.describe (Account2.open "bob" 10)) "bob: 10 USD"
```

``` ucm
scratch/main> update
scratch/main> test
scratch/main> todo
scratch/main> branches
```

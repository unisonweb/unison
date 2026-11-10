<!-- expected: behavior FAIL, outcome PASS, no-collateral PASS -->
`deposit` rebuilds the account with `"USD"` instead of keeping its currency, and the test that would show it was left out.

```` unison
unique type Account = { owner : Text, balance : Nat, currency : Text }

Account.open : Text -> Nat -> Account
Account.open owner balance = Account owner balance "USD"

Account.deposit : Nat -> Account -> Account
Account.deposit n a = Account (Account.owner a) (Account.balance a + n) "USD"

Account.describe : Account -> Text
Account.describe a = Account.owner a ++ ": " ++ Nat.toText (Account.balance a) ++ " " ++ Account.currency a

Account.sample : Account
Account.sample = Account.open "bob" 10

Account.audit : [Account] -> Text
Account.audit accounts = Text.join ", " (List.map Account.describe accounts)

Account.totalBalance : [Account] -> Nat
Account.totalBalance accounts = List.foldLeft (acc a -> acc + Account.balance a) 0 accounts

test> Account.open.tests.example = test.verify do
  ensureEqual (Account.open "ann" 5) (Account "ann" 5 "USD")

test> Account.deposit.tests.example = test.verify do
  ensureEqual (Account.deposit 7 Account.sample) (Account "bob" 17 "USD")

test> Account.describe.tests.example = test.verify do
  ensureEqual (Account.describe Account.sample) "bob: 10 USD"

test> Account.audit.tests.example = test.verify do
  ensureEqual (Account.audit [Account.sample, Account.open "ann" 5]) "bob: 10 USD, ann: 5 USD"

test> Account.totalBalance.tests.example = test.verify do
  ensureEqual (Account.totalBalance [Account.sample, Account.open "ann" 5]) 15
````

``` ucm
scratch/main> update
scratch/main> test
scratch/main> todo
scratch/main> branches
```

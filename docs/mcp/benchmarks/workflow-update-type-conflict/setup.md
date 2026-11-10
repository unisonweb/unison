A small accounts library: a record `Account`, functions that build, change and show accounts (`open`, `deposit` and the tests that build accounts with the constructor use it directly, so adding a field breaks them), two that work on lists of accounts, and a test for each.

```` unison
unique type Account = { owner : Text, balance : Nat }

Account.open : Text -> Nat -> Account
Account.open owner balance = Account owner balance

Account.deposit : Nat -> Account -> Account
Account.deposit n a = Account (Account.owner a) (Account.balance a + n)

Account.describe : Account -> Text
Account.describe a = Account.owner a ++ ": " ++ Nat.toText (Account.balance a)

Account.sample : Account
Account.sample = Account.open "bob" 10

Account.audit : [Account] -> Text
Account.audit accounts = Text.join ", " (List.map Account.describe accounts)

Account.totalBalance : [Account] -> Nat
Account.totalBalance accounts = List.foldLeft (acc a -> acc + Account.balance a) 0 accounts

test> Account.open.tests.example = test.verify do
  ensureEqual (Account.open "ann" 5) (Account "ann" 5)

test> Account.deposit.tests.example = test.verify do
  ensureEqual (Account.deposit 7 Account.sample) (Account "bob" 17)

test> Account.describe.tests.example = test.verify do
  ensureEqual (Account.describe Account.sample) "bob: 10"

test> Account.audit.tests.example = test.verify do
  ensureEqual (Account.audit [Account.sample, Account.open "ann" 5]) "bob: 10, ann: 5"

test> Account.totalBalance.tests.example = test.verify do
  ensureEqual (Account.totalBalance [Account.sample, Account.open "ann" 5]) 15
````

``` ucm
scratch/main> add
scratch/main> test
```

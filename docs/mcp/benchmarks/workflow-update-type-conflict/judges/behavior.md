You are judging the behavior of the `Account` functions in the Unison codebase you are connected to. A previous agent added a `currency : Text` field (last) to `Account`; every account that existed before is in `"USD"`. You may use any tool except `learn`. Don't change the project's definitions; use scratch code only for your own checks.

Evaluate each of these in a `unison` block of watch expressions (`> Account.describe (Account.open "bob" 10)` and so on) and compare with the expected value. `Account.currency.set "EUR" a` is the record setter, which gives an account in euros without relying on the constructor's argument order.

1. `Account.currency (Account.open "ann" 5)` is `"USD"`, and `Account.balance (Account.open "ann" 5)` is `5`.
2. `Account.describe (Account.open "bob" 10)` is `"bob: 10 USD"`, and `Account.describe Account.sample` is `"bob: 10 USD"`.
3. With `eve = Account.currency.set "EUR" (Account.open "eve" 1)`: `Account.describe (Account.deposit 3 eve)` is `"eve: 4 EUR"`, `Account.currency (Account.deposit 3 eve)` is `"EUR"`, `Account.balance (Account.deposit 3 eve)` is `4`.
4. `Account.audit [Account.open "bob" 10, Account.currency.set "EUR" (Account.open "ann" 5)]` is `"bob: 10 USD, ann: 5 EUR"`.
5. `Account.totalBalance [Account.open "bob" 10, Account.open "ann" 5]` is `15`.

It passes only if every value matches.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.

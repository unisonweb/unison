You are judging `Invoice.tax` and the functions that use it in the Unison codebase you are connected to. A previous agent changed `Invoice.tax` to round up to the next whole cent; it is 8% of an amount in cents. You may use any tool except `learn`. Don't change the project's definitions; use scratch code only for your own checks.

Evaluate each of these in a `unison` block of watch expressions (`> Invoice.tax 1005`, and so on) and compare with the expected value:

1. `Invoice.tax 1005` is `81`, `Invoice.tax 1` is `1`, `Invoice.tax 0` is `0`, `Invoice.tax 99` is `8` (7.92 rounded up), `Invoice.tax 1250` is `100` (exactly 100, not 101).
2. Amounts that divide exactly are unchanged: `Invoice.tax 1000` is `80` and `Invoice.tax 2500` is `200`.
3. `Invoice.total 1005` is `1086`, `Invoice.total 1000` is `1080`, `Invoice.total 1` is `2`.
4. `Invoice.describe 1005` is `"total: 1086 cents"` and `Invoice.describe 1000` is `"total: 1080 cents"`.

It passes only if every value matches.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.

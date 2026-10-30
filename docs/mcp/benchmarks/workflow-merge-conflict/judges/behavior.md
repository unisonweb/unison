You are judging the behavior of `Order.total` and `Order.summary` on the branch `alice` of the project `scratch` in the Unison codebase you are connected to. A previous agent merged the branch `bob` into it. You may use any tool except `learn`. Don't change the project's definitions or branches.

The intended behavior is the combination of two changes to `Order.total : Nat -> Nat`: take 10 off the subtotal and add a flat shipping fee of 5. So `Order.total subtotal` is `subtotal - 10 + 5`.

Run each of the following on `alice` (a transcript with the stanza `scratch/alice> ...` and a `unison` block with watch expressions such as `> Order.total 100`, whose value is printed) and check the results:

1. `Order.total 100` is `95`.
2. `Order.total 10` is `5`, `Order.total 20` is `15`, and `Order.total 50` is `45`. (These subtotals are at least 10, where `Nat` subtraction does not clamp at 0 and the order of the two operations makes no difference.)
3. `Order.summary 100` is `"Total: 95"` and `Order.summary 50` is `"Total: 45"`.
4. `view Order.total` has no conflict markers (`<<<<<<<`, `=======`, `>>>>>>>`).

It passes if every check passes. If a definition cannot be found or the code doesn't typecheck, that is a FAIL.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.

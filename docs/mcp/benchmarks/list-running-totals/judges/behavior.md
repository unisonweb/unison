You are judging `List.runningTotals` in the Unison codebase you are connected to. A previous agent wrote it, with tests. You may use any tool except `learn`. Don't change the project's definitions; use scratch code only for your own checks.

Run each of the following, and note the result of each:

1. `view List.runningTotals`: the type is `[Nat] -> [Nat]`.
2. `List.runningTotals [5, 0, 2]` is `[5, 5, 7]`.
3. `List.runningTotals []` is `[]`, and `List.runningTotals [7]` is `[7]` (no extra leading zero).
4. `List.last (List.runningTotals (List.range 0 100))` is `Some 4950`.

`List.runningTotals` passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.

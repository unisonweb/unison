You are judging `average` in the Unison codebase you are connected to. A previous agent wrote it, with tests. You may use any tool except `learn`. Don't change the project's definitions; use scratch code only for your own checks.

Run each of the following, and note the result of each:

1. `view average` and `view averageOrNone`: the types are `[Int] ->{Exception} Int` and `[Int] -> Optional Int`.
2. `average [2, 4, 6]` is `+4`, `average [7]` is `+7`, and `averageOrNone [3, 4]` is `Some +3` (integer division).
3. `averageOrNone []` is `None`.
4. `average []` fails with an `Exception` (a `Failure`), not a division by zero crash: evaluating `Exception.catch do average []` in a scratch file gives `Left` with a failure, and doesn't terminate the evaluation.

`average` passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.

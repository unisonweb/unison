You are judging `average` and `averageOrNone` in the Unison codebase you are connected to. A previous agent wrote them, with tests. You may use any tool except `learn`. Don't change the project's definitions; use scratch code only for your own checks.

Run each of the following, and note the result of each:

1. `view average` and `view averageOrNone`: `average` takes a `[Int]` and returns an `Int`, with whatever ability it uses to fail in its type (`Exception` is expected, but `Abort` or `Throw` also work); `averageOrNone : [Int] -> Optional Int`, with no abilities.
2. `average [+2, +4, +6]` is `+4` and `average [+7]` is `+7` (evaluate it under the handler that matches its type: `Exception.catch`, `Abort.toOptional!` or similar), and `averageOrNone [+3, +4]` is `Some +3` (integer division).
3. `averageOrNone []` is `None`.
4. `average []` fails (it is not a division-by-zero crash, a hang, or a number): under the matching handler it gives `Left`/`None`, and doesn't terminate the evaluation.

They pass only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.

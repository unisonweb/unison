You are judging `smallestMultiple` in the Unison codebase you are connected to. A previous agent wrote it, with tests. You may use any tool except `learn`. Don't change the project's definitions; use scratch code only for your own checks.

Run each of the following, and note the result of each:

1. `view smallestMultiple`: the type is `Nat -> Nat` (or a type that is compatible and returns the exact value).
2. `smallestMultiple 1` is `1`, `smallestMultiple 10` is `2520`, `smallestMultiple 20` is `232792560`.
3. `smallestMultiple 40` is `5342931457063200` (a brute-force search up the numbers would not finish).

`smallestMultiple` passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.

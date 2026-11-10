You are judging `shuffleSeeded` in the Unison codebase you are connected to. A previous agent wrote it, with tests. You may use any tool except `learn`. Don't change the project's definitions; use scratch code only for your own checks.

Run each of the following, and note the result of each:

1. `view shuffleSeeded`: the type is `Nat -> [a] -> [a]`, with no abilities (it is pure: no `IO`, no `Random` left unhandled).
2. For `List.range 0 20`, `shuffleSeeded 1 xs` called twice gives equal lists.
3. `List.sort (shuffleSeeded 7 xs) == xs` is true for `xs = List.range 0 20`.
4. Across seeds `1` to `10`, the results for `List.range 0 20` are not all the same (at least five distinct ones).
5. `shuffleSeeded 3 []` is `[]` and `shuffleSeeded 3 ["x"]` is `["x"]`.

`shuffleSeeded` passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.

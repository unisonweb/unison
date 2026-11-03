You are judging `List.runLengths` in the Unison codebase you are connected to. A previous agent wrote it, with tests. You may use any tool except `learn`. Don't change the project's definitions; use scratch code only for your own checks.

Run each of the following, and note the result of each:

1. `view List.runLengths`: the type is `[a] -> [(a, Nat)]` (it must work on any element type).
2. `List.runLengths [1, 1, 2, 3, 3, 3, 1]` is `[(1, 2), (2, 1), (3, 3), (1, 1)]`: equal values that are not adjacent do not merge.
3. `List.runLengths ["x"]` is `[("x", 1)]` and `List.runLengths ([] : [Nat])` is `[]`.
4. `List.size (List.runLengths (List.map (n -> n / 10) (List.range 0 100)))` is `10` (a list of 100 elements).

`List.runLengths` passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.

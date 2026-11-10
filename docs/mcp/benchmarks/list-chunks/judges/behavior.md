You are judging `List.chunks` in the Unison codebase you are connected to. A previous agent wrote it, with tests. You may use any tool except `learn`. Don't change the project's definitions; use scratch code only for your own checks.

Run each of the following, and note the result of each:

1. `view List.chunks`: the type is `Nat -> [a] -> [[a]]`.
2. `List.chunks 2 [1, 2, 3, 4, 5]` is `[[1, 2], [3, 4], [5]]`.
3. `List.chunks 3 ["a", "b", "c", "d", "e", "f"]` is `[["a", "b", "c"], ["d", "e", "f"]]`.
4. `List.chunks 10 [1, 2]` is `[[1, 2]]`, and `List.chunks 2 ([] : [Nat])` is `[]`.
5. `List.size (List.chunks 7 (List.range 0 100))` is `15`, and `List.last (List.chunks 7 (List.range 0 100))` is `Some [98, 99]`.

`List.chunks` passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.

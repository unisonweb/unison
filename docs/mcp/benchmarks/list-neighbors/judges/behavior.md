You are judging `List.neighbors` in the Unison codebase you are connected to. A previous agent wrote it, with tests. You may use any tool except `learn`. Don't change the project's definitions; use scratch code only for your own checks.

Run each of the following, and note the result of each:

1. `view List.neighbors`: the type is `[a] -> [(a, a)]`.
2. `List.neighbors ["a", "b", "c"]` is `[("a", "b"), ("b", "c")]`.
3. `List.neighbors [1]` and `List.neighbors ([] : [Nat])` are both `[]`.
4. `List.size (List.neighbors (List.range 0 100))` is `99`.

`List.neighbors` passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.

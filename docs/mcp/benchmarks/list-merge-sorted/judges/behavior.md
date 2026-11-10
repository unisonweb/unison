You are judging `List.mergeSorted` in the Unison codebase you are connected to. A previous agent wrote it, with tests. You may use any tool except `learn`. Don't change the project's definitions; use scratch code only for your own checks.

Run each of the following, and note the result of each:

1. `view List.mergeSorted`: the type is `[a] -> [a] -> [a]` (with `Ord` constraints expressed however Unison does; it must work for `Nat` and for `Text`).
2. `List.mergeSorted [1, 3, 5] [2, 4, 6]` is `[1, 2, 3, 4, 5, 6]`.
3. Equal elements are kept: `List.mergeSorted [1, 2, 2] [2, 3]` is `[1, 2, 2, 2, 3]`.
4. One side empty: `List.mergeSorted [] [1, 2]` and `List.mergeSorted [1, 2] []` are `[1, 2]`; both empty gives `[]`.
5. `List.mergeSorted ["a", "c"] ["b"]` is `["a", "b", "c"]`.

`List.mergeSorted` passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.

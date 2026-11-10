You are judging `wordIds` in the Unison codebase you are connected to. A previous agent wrote it, with tests. You may use any tool except `learn`. Don't change the project's definitions; use scratch code only for your own checks.

Run each of the following, and note the result of each:

1. `view wordIds`: the type is `[Text] -> [Nat]`, with no abilities.
2. `wordIds ["a", "b", "a", "c", "b"]` is `[0, 1, 0, 2, 1]`.
3. `wordIds ["x", "x", "x"]` is `[0, 0, 0]`, and `wordIds ["c", "b", "a"]` is `[0, 1, 2]`.
4. `wordIds []` is `[]`.
5. `wordIds (List.map Nat.toText (List.range 0 1000))` is `List.range 0 1000` (a thousand distinct words, checked with `==`).

`wordIds` passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.

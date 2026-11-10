You are judging `wordCounts` in the Unison codebase you are connected to. A previous agent wrote it, with tests. You may use any tool except `learn`. Don't change the project's definitions; use scratch code only for your own checks.

Run each of the following, and note the result of each:

1. `view wordCounts`: the type is `Text -> Map Text Nat`.
2. `Map.toList (wordCounts "The cat and the hat")` is `[("and", 1), ("cat", 1), ("hat", 1), ("the", 2)]`.
3. `wordCounts "  a\tA\n\nb   a "` has `"a"` -> 3 and `"b"` -> 1 (tabs, newlines and repeated spaces separate words; no empty-text key).
4. `wordCounts ""` is the empty map.

`wordCounts` passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.

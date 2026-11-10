You are judging `groupByLength` in the Unison codebase you are connected to. A previous agent wrote it, with tests. You may use any tool except `learn`. Don't change the project's definitions; use scratch code only for your own checks.

Run each of the following, and note the result of each:

1. `view groupByLength`: the type is `[Text] -> Map Nat [Text]`.
2. `Map.toList (groupByLength ["a", "bb", "c", "dd", "eee"])` is `[(1, ["a", "c"]), (2, ["bb", "dd"]), (3, ["eee"])]`.
3. Input order is preserved inside a group: `Map.get 2 (groupByLength ["zz", "aa", "mm"])` is `Some ["zz", "aa", "mm"]` (not reversed, not sorted).
4. Duplicates are kept: `Map.get 1 (groupByLength ["a", "a"])` is `Some ["a", "a"]`.
5. `groupByLength []` is the empty map.

`groupByLength` passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.

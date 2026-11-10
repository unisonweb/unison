You are judging `Map.mergeMax` in the Unison codebase you are connected to. A previous agent wrote it, with tests. You may use any tool except `learn`. Don't change the project's definitions; use scratch code only for your own checks.

Run each of the following, and note the result of each:

1. `view Map.mergeMax`: the type is `Map Text Nat -> Map Text Nat -> Map Text Nat`.
2. `Map.toList (Map.mergeMax (Map.fromList [("a", 3), ("b", 9)]) (Map.fromList [("b", 4), ("c", 1)]))` is `[("a", 3), ("b", 9), ("c", 1)]`.
3. The same with the arguments swapped gives the same list.
4. `Map.toList (Map.mergeMax Map.empty (Map.fromList [("x", 0)]))` is `[("x", 0)]`, and merging two empty maps gives `[]`.
5. `Map.toList (Map.mergeMax (Map.fromList [("k", 5)]) (Map.fromList [("k", 5)]))` is `[("k", 5)]`.

`Map.mergeMax` passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.

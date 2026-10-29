You are judging `Map.addCounts` in the Unison codebase you are connected to. A previous agent wrote it, with tests. You may use any tool except `learn`. Don't change the project's definitions; use scratch code only for your own checks.

Run each of the following, and note the result of each:

1. `view Map.addCounts`: the type is `Map k Nat -> Map k Nat -> Map k Nat`.
2. `Map.toList (Map.addCounts (Map.fromList [("a", 1), ("b", 2)]) (Map.fromList [("b", 5), ("c", 1)]))` is `[("a", 1), ("b", 7), ("c", 1)]`.
3. Adding the empty map on either side leaves the other unchanged; two empty maps give the empty map.
4. Keys of another type work: `Map.addCounts (Map.fromList [(1, 4)]) (Map.fromList [(1, 6)])` has `1` -> 10.

`Map.addCounts` passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.

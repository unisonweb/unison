You are judging `Map.invert` in the Unison codebase you are connected to. A previous agent wrote it, with tests. You may use any tool except `learn`. Don't change the project's definitions; use scratch code only for your own checks.

Run each of the following, and note the result of each:

1. `view Map.invert`: the type is `Map k v -> Map v [k]`.
2. `Map.toList (Map.invert (Map.fromList [(1, "a"), (2, "b"), (3, "a")]))` is `[("a", [1, 3]), ("b", [2])]`.
3. Ascending order is kept with several keys: inverting `Map.fromList [(1, 0), (2, 0), (3, 0), (4, 0)]` gives `{0 -> [1, 2, 3, 4]}`.
4. The empty map inverts to the empty map.
5. Inverting `Map.fromList (List.map (n -> (n, n / 10)) (List.range 0 100))` has 10 entries, each a list of 10 keys.

`Map.invert` passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.

You are judging `parseAll` in the Unison codebase you are connected to. A previous agent wrote it, with tests. You may use any tool except `learn`. Don't change the project's definitions; use scratch code only for your own checks.

Run each of the following, and note the result of each:

1. `view parseAll`: the type is `[Text] -> Either Text [Nat]`.
2. `parseAll ["1", "2", "3"]` is `Right [1, 2, 3]` and `parseAll ["1", "x", "y"]` is `Left "x"` (the first bad one, not the last).
3. `parseAll []` is `Right []`.
4. `parseAll ["-1"]` is `Left "-1"` (a `Nat` can't be negative), `parseAll ["12a"]` is `Left "12a"` and `parseAll ["1", ""]` is `Left ""`.
5. A list of 100 numeric texts gives `Right` with all 100.

`parseAll` passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.

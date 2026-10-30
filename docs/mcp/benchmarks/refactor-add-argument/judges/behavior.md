You are judging the behavior of `bullets` and of the code that uses it in the Unison codebase you are connected to. A previous agent gave `bullets` a new first argument, a limit. You may use any tool except `learn`. Don't change the project's definitions; use `unison` blocks of watch expressions (`> expr`) for your own checks and don't `update` them. Work in `scratch/main`.

`bullets limit items` shows at most `limit` items, each as a line `- item`, joined with newlines, and when some are left out the output ends with one more line, `... and N more`, where `N` is how many were left out.

Evaluate each of these and compare with the expected value:

1. `bullets 2 ["a", "b", "c", "d"]` is `"- a\n- b\n... and 2 more"`.
2. `bullets 4 ["a", "b", "c", "d"]` is `"- a\n- b\n- c\n- d"` and `bullets 9 ["a"]` is `"- a"` (nothing left out, so no extra line).
3. `bullets 3 []` is `""`. `bullets 0 ["a", "b"]` is `"... and 2 more"`. `bullets 1 ["x", "y"]` is `"- x\n... and 1 more"`.
4. The code that used `bullets` behaves as before, with no limit: `Report.team ["ann", "bo"]` is `"Team\n- ann\n- bo"`; `Report.shopping ["Milk", "EGGS"]` is `"- milk\n- eggs"`; `Report.allSections [["x"], ["y", "z"]]` is `["- x", "- y\n- z"]`; `Report.sideBySide ["x"] ["y"]` is `"- x\n\n- y"`; `Report.trimmed [" a ", "b "]` is `"- a\n- b"`; `Report.bulletCount ["p", "q", "r"]` is `3`.
5. The old behavior holds for long lists, so the callers truly pass no limit: `Report.team (List.map Nat.toText (List.range 0 2000))` has `Text.size` 12894 and does not contain the text `more`; `Report.shopping (List.map Nat.toText (List.range 0 150))` does not contain the text `more`.
6. A partial application works: `List.map (bullets 1) [["a", "b"], ["c"]]` is `["- a\n... and 1 more", "- c"]`.

It passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.

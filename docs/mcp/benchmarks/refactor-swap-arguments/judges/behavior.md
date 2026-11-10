You are judging the behavior of `Log.line` and of the code that uses it in the Unison codebase you are connected to. A previous agent swapped the first two arguments of `Log.line`, so that it is `Log.line code level message`. You may use any tool except `learn`. Don't change the project's definitions; use `unison` blocks of watch expressions (`> expr`) for your own checks and don't `update` them. Work in `scratch/main`.

Evaluate each of these and compare with the expected value:

1. `Log.line 42 "WARN" "disk low"` is `"[WARN 42] disk low"`, and `Log.line 0 "" ""` is `"[ 0] "`.
2. `Log.startup` is `"[INFO 1] started"`; `Log.fatal` is `"[FATAL 9] disk gone"`; `Log.bye "see you"` is `"[INFO 0] see you"`.
3. `Log.failure 503 "timeout"` is `"[ERROR 1503] failed: timeout"` and `Log.failure 404 "missing"` is `"[WARN 1404] failed: missing"`.
4. `Log.errors ["a", "b"]` is `["[ERROR 500] a", "[ERROR 500] b"]`; `Log.batch ["x", "y"]` is `["[INFO 7] x", "[INFO 7] y"]`.
5. `Log.warn 3 "careful"` is `"[WARN 3] careful"` and `Log.numbered [1, 2] ["p", "q"]` is `["[NOTE 1] p", "[NOTE 2] q"]`.
6. `Log.fromTriples [("DEBUG", 5, "x"), ("TRACE", 6, "y")]` is `["[DEBUG 5] x", "[TRACE 6] y"]`.
7. A partial application works in the new order: `List.map (Log.line 1 "T") ["m", "n"]` is `["[T 1] m", "[T 1] n"]`.
8. The unrelated `Table.line` is as before: `Table.header` is `"Name........"` and `Table.rows ["a", "b"]` is `["a...", "b..."]`.

It passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.

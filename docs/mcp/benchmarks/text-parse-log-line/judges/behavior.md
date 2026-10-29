You are judging `LogLine.parse` in the Unison codebase you are connected to. A previous agent wrote it, with tests. You may use any tool except `learn`. Don't change the project's definitions; use scratch code only for your own checks.

Run each of the following, and note the result of each:

1. `view LogLine` and `view LogLine.parse`: `LogLine` is a record with `level : Text`, `hour : Nat`, `minute : Nat`, `message : Text`, and the type of `LogLine.parse` is `Text -> Optional LogLine`.
2. `LogLine.parse "[ERROR] 12:30 disk full"` is `Some` of a `LogLine` with the four fields as in the task.
3. `LogLine.parse "[INFO] 07:05 started: ok [x]"` has level `"INFO"`, hour `7`, minute `5` and message `"started: ok [x]"` (the message may contain `:` and brackets).
4. `LogLine.parse "garbage"`, `LogLine.parse ""` and `LogLine.parse "[INFO] 12 no minutes"` are `None`.

`LogLine.parse` passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.

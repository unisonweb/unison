You are judging `Log.collect` in the Unison codebase you are connected to. A previous agent wrote it, with tests. You may use any tool except `learn`. Don't change the project's definitions; use scratch code only for your own checks.

Run each of the following, and note the result of each:

1. `view Log` and `view Log.collect`: `Log` is an ability with `log : Text -> ()`, and `Log.collect` runs a computation and returns a pair.
2. In a scratch file: `Log.collect do log "start"; log "end"; 42` is `(42, ["start", "end"])`.
3. `Log.collect do 7` is `(7, [])`.
4. Messages stay in order after many: logging `"0"` to `"99"` in a loop gives 100 messages in order.

`Log.collect` passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.

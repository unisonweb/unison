You are judging whether a change kept the rest of the Unison codebase you are connected to. A previous agent added a `port : Nat` field to `Config`, threaded the port through a chain of three functions, and gave a second chain of four functions (`Backoff.delay`, `Job.wait`, `Job.total`, `Job.within`) a new first argument, the base delay, together with their tests. Everything that existed before must still exist and behave as before, apart from what the change asked for. You may use any tool except `learn`. Don't change the project's definitions.

Check, in project `scratch` on branch `main`:

1. `find` and `ls`: the project contains exactly the definitions of `Config` (the type, its constructor, accessors `host`, `retries`, `port` with their `.set` and `.modify`), `Config.parse`, `Server.connect`, `Server.status`, `Backoff.delay`, `Job.wait`, `Job.total`, `Job.within` and the seven tests `Config.parse.tests.example`, `Server.connect.tests.example`, `Server.status.tests.example`, `Backoff.delay.tests.example`, `Job.wait.tests.example`, `Job.total.tests.example`, `Job.within.tests.example`, plus `lib`. A missing definition (especially `Job.total`, `Job.within` or a test, which are lost when `update` saves a file from which they were left out) or an extra copy under a new name is a failure. Extra tests are fine.
2. `view Server.connect`: it still builds its answer from the `Config` that `Config.parse` returns (the host, port and retries come from the record), not from concatenating its arguments by hand.
3. `view Server.status Job.wait Job.total Job.within`: `status` adds `": ok"` to the result of `connect`; `wait` is the delay of `Backoff.delay`; `total` adds up the waits for attempts `0` to `attempts - 1`; `within` compares that total with the limit: the logic is unchanged apart from carrying the new argument.
4. `view Backoff.delay`: it is a power of two times the base (`base * 2^attempt`), not a table or a loop that gives the same values for a few inputs only.
5. `view Config.parse.tests.example Server.connect.tests.example Server.status.tests.example Backoff.delay.tests.example Job.wait.tests.example Job.total.tests.example Job.within.tests.example`: each still checks a value (`ensureEqual` or similar) with the expected value written out, with the port in the text where the change asks for it, and none was deleted or weakened to `ensure true`.
6. `list lib`: `lib.unison_base_7_20_0` is the only library.

It passes if every check holds. It fails if any one does not.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.

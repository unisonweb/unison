You are judging whether a change kept the rest of the Unison codebase you are connected to. A previous agent added a `port : Nat` field to `Config` and threaded the port through a chain of four functions and their tests. Everything that existed before must still exist and behave as before, apart from what the change asked for. You may use any tool except `learn`. Don't change the project's definitions.

Check, in project `scratch` on branch `main`:

1. `find` and `ls`: the project contains exactly the definitions of `Config` (the type, its constructor, accessors `host`, `retries`, `port` with their `.set` and `.modify`), `Config.parse`, `Server.connect`, `Server.status`, `Server.report` and the four tests `Config.parse.tests.example`, `Server.connect.tests.example`, `Server.status.tests.example`, `Server.report.tests.example`, plus `lib`. A missing definition (especially `Server.status`, `Server.report` or a test, which are lost when `update` saves a file from which they were left out) or an extra copy under a new name is a failure. Extra tests are fine.
2. `view Server.connect`: it still builds its answer from the `Config` that `Config.parse` returns (the host, port and retries come from the record), not from concatenating its arguments by hand.
3. `view Server.status Server.report`: `status` adds `": ok"` to the result of `connect`, and `report` applies `status` to each host and joins the results with a newline: the logic is unchanged apart from carrying the port.
4. `view Config.parse.tests.example Server.connect.tests.example Server.status.tests.example Server.report.tests.example`: each still checks a value (`ensureEqual` or similar) with the expected text written out, with the port in it where the prompt asks for it, and none was deleted or weakened to `ensure true`.
5. `list lib`: `lib.unison_base_7_20_0` is the only library.

It passes if every check holds. It fails if any one does not.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.

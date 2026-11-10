You are judging the end state of the project in the Unison codebase you are connected to. A previous agent was asked to swap the first two arguments of `Log.line`, so that `Log.line : Nat -> Text -> Text -> Text` takes the numeric code, then the level, then the message, and to update everything that uses it so that nothing changes for them. You may use any tool except `learn`. Don't change anything in the codebase (no `update`, `switch` or edits); read it with transcripts that only run read commands. A `unison` block of watch expressions (`> expr`) is fine for evaluating things.

Setup, so you know what to expect: the project `scratch`, branch `main`, had `Log.line : Text -> Nat -> Text -> Text` and these definitions that use it: `Log.batch`, `Log.bye`, `Log.errors`, `Log.failure`, `Log.fatal`, `Log.fromTriples`, `Log.line.doc` (its `docExample` and its `@eval`), `Log.numbered`, `Log.startup`, `Log.warn`, and the tests `Log.tests.line`, `Log.tests.failure`, `Log.tests.partial`, `Log.tests.piped`. Also `Log.levelFor`, and a separate function `Table.line : Text -> Nat -> Text` with `Table.header`, `Table.rows` and `Table.tests.line`.

Run these and check each:

1. A bare `> reflog` stanza (no project prefix) lists the changes of the current branch. Its Branch column must say `scratch/main`: the worker must end on `main`.
2. `scratch/main> branches` lists exactly `main`: no temporary branch (`update-main` or similar) is left.
3. `scratch/main> todo` has nothing pending.
4. `scratch/main> find Log.line` shows `Log.line : Nat -> Text -> Text -> Text`. `scratch/main> find` (everything outside `lib`) shows exactly the 20 definitions named above (16 under `Log`, 4 under `Table`), and no other: no helper such as `Log.lineSwapped`, no rewrite rule such as `swap`, no old copy of `Log.line`. New tests are allowed (names containing `tests`).
5. `scratch/main> view` the callers and check, for each, that the arguments are in the new order (a code, then a level, then a message), with nothing else changed: `Log.startup` is `Log.line 1 "INFO" "started"`; `Log.errors` passes `Log.line 500 "ERROR"` to `List.map`; `Log.batch` uses `Log.line 7 "INFO"` after `|>`; `Log.bye` uses `Log.line 0 "INFO"` after `|>`; `Log.fatal` uses `Log.line 9 "FATAL"` before `<|`; `Log.failure` passes `(code + 1000)` first and `(Log.levelFor code)` second; `Log.warn` and `Log.numbered`, which used `Log.line` with only the level, now put the code before `"WARN"` and `"NOTE"` (for example `Log.warn code message = Log.line code "WARN" message`, or a lambda or composition that does the same); `Log.fromTriples` calls `Log.line code level message`.
6. `scratch/main> view Log.line.doc` has a `docExample` and an `@eval` that call `Log.line` in the new order (a number first), and `scratch/main> docs Log.line` renders without an error message and shows `"[WARN 42] disk low"`.
7. `scratch/main> test` runs and every test passes (at least the five from the setup: `Log.tests.line`, `Log.tests.failure`, `Log.tests.partial`, `Log.tests.piped`, `Table.tests.line`), nothing fails, and none of them was deleted or emptied (view each: they still check text).

It passes if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.

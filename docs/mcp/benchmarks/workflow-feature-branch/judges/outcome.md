You are judging where the work ended up in the Unison codebase you are connected to. The project `scratch` had one branch, `main`, with `Text.whisper : Text -> Text` and its test `Text.whisper.tests.basic`. A previous agent was told to add `Text.shout : Text -> Text` with a test on a branch called `shout`, then bring it back to `main`, leave `main` as the current branch, and leave no `shout` branch behind. You may use any tool except `learn`. Don't change the project's definitions; use scratch code only for your own checks.

Run each of the following and note the result of each. Start with the first check, before any command that would change which branch you are on, and don't switch branches:

1. A transcript whose first stanza is a bare `> reflog` (a command line with no project or branch in front of it) lists the changes of the current branch. Its Branch column must say `scratch/main`: the agent had to end on `main`.
2. `branches` lists `main` and nothing else: there is no `shout` branch, and no temporary branch such as `merge-shout-into-main` or `update-main`.
3. `todo` reports nothing pending.
4. On `main`: `view Text.shout` shows a function of type `Text -> Text`, and `ls Text.shout` or `find tests` shows at least one test of it in the project (not in a library) whose expected value is written out. `test` runs all tests, including `Text.whisper.tests.basic` and the test of `shout`, with no failure.
5. `view Text.whisper` is still `Text.toLowercase t` (nothing else changed) and `Text.whisper.tests.basic` is still there and unchanged.
6. `history` of `main` shows the original definitions of `Text.whisper` and, after them, the added `Text.shout` and its test; no other definition of the project was added or removed (`ls` of the project's own namespace shows `Text.shout`, `Text.whisper`, and nothing unrelated; `lib` has only `@unison/base`).

It passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.

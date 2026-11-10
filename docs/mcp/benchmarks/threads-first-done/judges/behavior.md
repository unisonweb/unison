You are judging `firstDone` in the Unison codebase you are connected to. A previous agent wrote it, with tests. You may use any tool except `learn`. Don't change the project's definitions; use scratch code only for your own checks.

Run each of the following, and note the result of each:

1. `view firstDone`: it takes a `[Nat]` and returns a `Nat`, and it executes the effects (it does not return a thunk), so its type lists `IO`. It may list `Threads` too, or it may run the threads itself with `Threads.run` inside; either is fine.
2. Run `IO.Threads.run do firstDone [300, 50, 900]` and check the result is `50`. Programs that use `IO` need the transcript tool that allows IO (`transcript-io`); run them as `> IO.Threads.run do ...` or with `run`, and `io.test` for IO tests.
3. Run `IO.Threads.timeout` (about 1 second; find the `Duration` constructors with `find`) around `firstDone [5000, 100, 8000]`: it returns `100` inside the timeout, so the slow tasks are cancelled rather than waited for.
4. `firstDone [10]` is `10`. `firstDone []` fails with an `Exception` (check with `Exception.catch`, inside a `IO.Threads.timeout` of a few seconds): it must raise, not hang and not return a number.

`firstDone` passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.

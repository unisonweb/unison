You are judging `timedPair` in the Unison codebase you are connected to. A previous agent wrote it, with tests. You may use any tool except `learn`. Don't change the project's definitions; use scratch code only for your own checks.

Run each of the following, and note the result of each:

1. `view timedPair`: it takes two `Nat`s and returns a pair of them, and it executes the effects (it is not a function that returns a thunk), so its type lists `IO`. It may list `Threads` too, or it may run the threads itself with `Threads.run` inside; either is fine.
2. Run `timedPair 300 200` and check the result is `(300, 200)`. Programs that use `IO` need the transcript tool that allows IO (`transcript-io`); run them as `> IO.Threads.run do ...` or with `run`, and `io.test` for IO tests.
3. Time it: around `timedPair 400 400` take `Clock.monotonic()` before and after (find the `Duration` functions with `find`); it finishes in under 700 milliseconds (sequential waiting would take 800).
4. `timedPair 0 0` is `(0, 0)`, and `timedPair 50 0` is `(50, 0)`.

`timedPair` passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.

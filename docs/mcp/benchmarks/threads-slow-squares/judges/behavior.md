You are judging `slowSquares` in the Unison codebase you are connected to. A previous agent wrote it, with tests. You may use any tool except `learn`. Don't change the project's definitions; use scratch code only for your own checks.

Run each of the following, and note the result of each:

1. `view slowSquares`: it takes a `[Nat]` and returns a `[Nat]`, and its type lists the abilities it needs, including `IO` and `Threads` (it executes the effects; it is not a function that returns a thunk).
2. Run `IO.Threads.run do slowSquares [1, 2, 3]` and check the result is `[1, 4, 9]`. Programs that use `IO` need the transcript tool that allows IO (`transcript-io`); run them as `> IO.Threads.run do ...` or with `run`, and `io.test` for IO tests.
3. Run `IO.Threads.timeout (Duration.seconds 2)`-style around `slowSquares (List.range 0 20)` (find the right `Duration` constructor with `find`): it finishes inside the timeout (sequential sleeping would take four seconds) and gives the squares in input order.
4. `slowSquares []` is `[]`.

`slowSquares` passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.

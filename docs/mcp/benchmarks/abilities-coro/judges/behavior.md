You are judging `Coro.feed` in the Unison codebase you are connected to. A previous agent wrote it, with tests. You may use any tool except `learn`. Don't change the project's definitions; use scratch code only for your own checks.

Run each of the following, and note the result of each:

1. `view Coro` and `view Coro.feed`: `Coro` is an ability with `yield : a -> b`, and `Coro.feed` takes a stream producer, then a coroutine, and returns a delayed stream (a function with no arguments, like `'{Stream a} ()` or similar).
2. Define in a scratch file a running-sum coroutine as in the task (`go acc = go (acc + Coro.yield acc)`, started with `go 0`) and a producer `Stream.fromList [1, 2, 3, 4]`. `Stream.toList (Coro.feed producer coroutine)` is `[0, 1, 3, 6, 10]`.
3. A second coroutine of type `Coro Text Nat`: it yields `"start"`, then forever yields `Nat.toText` of each reply it receives. Fed a producer of `10, 20`, the output stream is `["start", "10", "20"]`.
4. An empty producer gives the output of the coroutine up to its first yield and then ends: `Stream.toList (Coro.feed Stream.empty coroutine)` is `[0]` for the running-sum coroutine.
5. A coroutine that finishes by itself before the producer runs out (it yields once and returns) gives a one-element stream and doesn't loop or error.

`Coro.feed` passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.

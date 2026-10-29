In the current Unison project, I need an ability `Coro a b` with one operation `yield : a -> b`: a coroutine sends an `a` out and receives a `b` back.

I also need `Coro.feed`, which composes a producer `Stream` of `b` values with a computation that uses `Coro a b`, into a `Stream` of the `a` values the coroutine yields. Each time the coroutine does `yield x`, `x` is emitted on the resulting stream, and the reply to the `yield` is the next element of the producer. When the producer runs out, the resulting stream ends. `Coro.feed` takes the producer first and the coroutine second.

For example, take a coroutine that keeps a running sum: it starts at `0`, yields the sum, adds the reply to it, and repeats. Fed with a producer of `1, 2, 3, 4`, the resulting stream is `0, 1, 3, 6, 10`. Add it to the codebase, with tests.

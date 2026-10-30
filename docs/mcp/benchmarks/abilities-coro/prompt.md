In the current Unison project, I need an ability `Coro a b` with one operation `yield : a -> b`: a coroutine sends an `a` out and receives a `b` back.

I also need `Coro.feed`, which takes a `Stream` of `b` values and a computation that uses `Coro a b` (the stream first), and returns a `Stream` of `a` values. For example, take a coroutine that keeps a running sum: it starts at `0`, yields the sum, adds the reply to it, and repeats. Fed with a stream of `1, 2, 3, 4`, the resulting stream is `0, 1, 3, 6, 10`. Add it to the codebase, with tests.

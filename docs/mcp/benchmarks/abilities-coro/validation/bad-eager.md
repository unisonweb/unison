``` unison
ability Coro a b where
  yield : a -> b

Coro.feed : '{g, Stream b} r -> '{g, Coro a b} r2 -> '{g, Stream a} ()
Coro.feed producer coroutine = do
  items = Stream.toList producer
  step : [b] -> Request {Coro a b} r2 ->{g, Stream a} ()
  step rest = cases
    { _ } -> ()
    { Coro.yield x -> resume } ->
      Stream.emit x
      match rest with
        [] -> ()
        b +: rest' -> handle resume b with step rest'
  handle coroutine() with step items

Coro.tests.runningSum : '{Coro Nat Nat} Void
Coro.tests.runningSum = do
  go acc = go ((Nat.+) acc (Coro.yield acc))
  go 0

Coro.tests.labels : '{Coro Text Nat} Void
Coro.tests.labels = do
  go = do
    n = Coro.yield "start"
    loop n
  loop n =
    m = Coro.yield (Nat.toText n)
    loop m
  go()

Coro.tests.once : '{Coro Nat Nat} Nat
Coro.tests.once = do
  n = Coro.yield 1
  n + 98

test> Coro.feed.tests.example = test.verify do
  ensureEqual (Stream.toList (Coro.feed (Stream.fromList [1, 2, 3, 4]) Coro.tests.runningSum)) [0, 1, 3, 6, 10]
  ensureEqual (Stream.toList (Coro.feed Stream.empty Coro.tests.runningSum)) [0]
  ensureEqual (Stream.toList (Coro.feed (Stream.fromList [10, 20]) Coro.tests.once)) [1]
```
``` ucm
scratch/main> add
scratch/main> test
```

``` unison
ability Coro a b where
  yield : a -> b

Coro.feed : '{g, Stream b} r -> '{g, Coro a b} r2 -> '{g, Stream a} ()
Coro.feed producer coroutine = do
  step : '{g, Stream b} r -> Request {Coro a b} r2 ->{g, Stream a} ()
  step prod = cases
    { _ } -> ()
    { Coro.yield x -> resume } ->
      Stream.emit x
      match Stream.uncons prod with
        Left _ -> ()
        Right (b, rest) -> ()
  handle coroutine() with step producer

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
  ensureEqual (Stream.toList (Coro.feed Stream.empty Coro.tests.runningSum)) [0]
  ensureEqual (Stream.toList (Coro.feed (Stream.fromList [10, 20]) Coro.tests.once)) [1]
```
``` ucm
scratch/main> add
scratch/main> test
```

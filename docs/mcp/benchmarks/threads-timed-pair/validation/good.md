``` unison
timedPair : Nat -> Nat ->{IO, Exception} (Nat, Nat)
timedPair a b =
  wait n =
    Threads.sleep (Duration.milliseconds (Nat.toInt n))
    n
  Threads.run do Threads.both (do wait a) (do wait b)

timedPair.tests.overlap : '{IO, Exception} [Result]
timedPair.tests.overlap = do test.verify do
  start = Clock.monotonic()
  result = timedPair 300 200
  elapsed = Clock.monotonic() - start
  ensureEqual result (300, 200)
  ensure (elapsed < Duration.milliseconds +450)
  ensureEqual (timedPair 0 0) (0, 0)
```
``` ucm
scratch/main> add
scratch/main> io.test timedPair.tests.overlap
```

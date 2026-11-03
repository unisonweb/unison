``` unison
firstDone : [Nat] ->{IO, Exception, Threads} Nat
firstDone delays =
  task d = do
    Threads.sleep (Duration.milliseconds (Nat.toInt d))
    d
  results = Threads.parMap (d -> task d ()) delays
  Optional.getOrBug "empty" (List.minimum results)

firstDone.tests.example : '{IO, Exception} [Result]
firstDone.tests.example = do test.verify do
  ensureEqual (Threads.run do firstDone [300, 50, 900]) 50
  ensureEqual (Threads.run do firstDone [10]) 10
```
``` ucm
scratch/main> add
scratch/main> io.test firstDone.tests.example
```

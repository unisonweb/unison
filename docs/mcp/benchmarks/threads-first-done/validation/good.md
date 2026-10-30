``` unison
firstDone : [Nat] ->{IO, Exception, Threads} Nat
firstDone delays =
  task d = do
    Threads.sleep (Duration.milliseconds (Nat.toInt d))
    d
  race = do Threads.multiRace (submit -> List.foreach (d -> submit (task d)) delays)
  if List.isEmpty delays then Exception.raiseGeneric "firstDone of no tasks" delays else race()

firstDone.tests.example : '{IO, Exception} [Result]
firstDone.tests.example = do test.verify do
  ensureEqual (Threads.run do firstDone [300, 50, 900]) 50
  ensureEqual (Threads.run do Threads.timeout (Duration.seconds +1) do firstDone [5000, 100, 8000]) 100
  ensureEqual (Threads.run do firstDone [10]) 10
  ensure (Either.isLeft (Threads.run do Exception.catch do firstDone []))
```
``` ucm
scratch/main> add
scratch/main> io.test firstDone.tests.example
```

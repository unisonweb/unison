```` unison
firstDone.docs.examples.race : '{IO, Exception} Nat
firstDone.docs.examples.race = do Threads.run do firstDone [300, 50, 900]

firstDone.doc = {{
{{ docExample 1 (do (delays -> firstDone delays)) }} starts one task per delay, each sleeping for its delay in milliseconds and then returning it, and returns the result of whichever finishes first. The other tasks are cancelled, so it returns after about the shortest delay. It fails with an `Exception` if `delays` is empty. It needs the `IO`, `Exception` and `Threads` abilities, so run it inside {IO.Threads.run}. For example:

@source{firstDone.docs.examples.race}

Run it in UCM with:

```raw
run firstDone.docs.examples.race
```

which returns

@typecheck ```
50
```

*Also see:* {IO.Threads.race}, {IO.Threads.multiRace}
}}

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
````
``` ucm
scratch/main> add
scratch/main> docs firstDone
scratch/main> io.test firstDone.tests.example
```
